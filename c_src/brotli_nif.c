/*
 * Copyright (c) 2016 Jihyun Yu <yjh0502@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <string.h>
#include <erl_nif.h>
#include <vector>
#include "include/brotli/encode.h"
#include "include/brotli/decode.h"

#define BADARG             enif_make_badarg(env)

static ERL_NIF_TERM
brotli_encode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int quality;
    ErlNifBinary data, encoded;

    if (argc != 2) {
        return (BADARG);
    }

    if (!enif_inspect_iolist_as_binary(env, argv[0], &data)) {
        return (BADARG);
    }

    if (!enif_get_uint(env, argv[1], &quality)) {
        return (BADARG);
    }

    size_t length = data.size;
    size_t output_length = BrotliEncoderMaxCompressedSize(data.size);
    uint8_t* output = (uint8_t*)malloc(output_length);
    BROTLI_BOOL ok = BrotliEncoderCompress(quality,
        BROTLI_DEFAULT_WINDOW, BROTLI_DEFAULT_MODE,
        length, data.data,
        &output_length, output);
    if(!ok) {
        free(output);
        return (BADARG);
    }
    if (!enif_alloc_binary(output_length, &encoded)) {
        free(output);
        return (BADARG);
    }

    memcpy(encoded.data, output, output_length);
    free(output);

    return enif_make_binary(env, &encoded);
}

  static ERL_NIF_TERM
brotli_decode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary data, decoded;
  const uint8_t* next_in;
  size_t available_in;
  int ok;

  if (argc != 1) {
    return (BADARG);
  }

  if (!enif_inspect_iolist_as_binary(env, argv[0], &data)) {
    return (BADARG);
  }

  std::vector<uint8_t> output;

  BrotliDecoderState* state = BrotliDecoderCreateInstance(0, 0, 0);

  BrotliDecoderResult result = BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT;
  next_in = static_cast<uint8_t*>(data.data);
  available_in = data.size;

  while (result == BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT) {
    size_t available_out = 0;
    result = BrotliDecoderDecompressStream(state, &available_in, &next_in,
                                           &available_out, 0, 0);
    const uint8_t* next_out = BrotliDecoderTakeOutput(state, &available_out);
    if (available_out != 0)
      output.insert(output.end(), next_out, next_out + available_out);
  }
  ok = result == BROTLI_DECODER_RESULT_SUCCESS && !available_in;
  BrotliDecoderDestroyInstance(state);

  if(!ok) {
    return (BADARG);
  }
  if (!enif_alloc_binary(output.size(), &decoded)) {
    return (BADARG);
  }

  /* memcpy(decoded.data, &buffer, (1 << 26)); */
  /* free(buffer); */
  std::copy(output.begin(), output.end(), decoded.data);

  return enif_make_binary(env, &decoded);
}

  static int
brotli_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
  return 0;
}

static ErlNifFunc brotli_exports[] = {
  {"brotli_decode", 1, brotli_decode},
  {"brotli_encode", 2, brotli_encode}
};

ERL_NIF_INIT(brotli_nif, brotli_exports, brotli_load, NULL, NULL, NULL)
