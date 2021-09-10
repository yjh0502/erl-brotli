/*
 * Copyright (c) 2016 Jihyun Yu <yjh0502@gmail.com>
 * Copyright (c) 2021 Łukasz Niemier <lukasz@niemier.pl>
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
#include <stdio.h>
#include <erl_nif.h>
#include "common/version.h"
#include "include/brotli/encode.h"
#include "include/brotli/decode.h"

/* ATOMS {{{ */
#define PARAMS \
  ENTRYV(mode, BROTLI_PARAM_MODE) \
  ENTRYV(quality, BROTLI_PARAM_QUALITY) \
  ENTRYV(window, BROTLI_PARAM_LGWIN) \
  ENTRYV(block_size, BROTLI_PARAM_LGBLOCK) \
  ENTRYV(literal_context_modeling, BROTLI_PARAM_DISABLE_LITERAL_CONTEXT_MODELING) \
  ENTRYV(size_hint, BROTLI_PARAM_SIZE_HINT) \
  ENTRYV(large_window, BROTLI_PARAM_LARGE_WINDOW) \
  ENTRYV(npostfix, BROTLI_PARAM_NPOSTFIX) \
  ENTRYV(ndirect, BROTLI_PARAM_NDIRECT) \
  ENTRYV(stream_offset, BROTLI_PARAM_STREAM_OFFSET)

#define OPERATIONS \
  ENTRYV(process, BROTLI_OPERATION_PROCESS) \
  ENTRYV(flush, BROTLI_OPERATION_FLUSH) \
  ENTRYV(finish, BROTLI_OPERATION_FINISH) \
  ENTRYV(emit_metadata, BROTLI_OPERATION_EMIT_METADATA)

#define MODES \
  ENTRYV(generic, BROTLI_MODE_GENERIC) \
  ENTRYV(text, BROTLI_MODE_TEXT) \
  ENTRYV(font, BROTLI_MODE_FONT)

#define BOOLS \
  ENTRYV(true, BROTLI_TRUE) \
  ENTRYV(false, BROTLI_FALSE)

#define ATOMS \
  ENTRY(ok) \
  ENTRY(error) \
  ENTRY(more) \
  ENTRY(finished)

#define ENTRY(X) static ERL_NIF_TERM atom_##X;
ATOMS;
#undef ENTRY
#define ENTRYV(X, V) static ERL_NIF_TERM atom_##X;
PARAMS;
OPERATIONS;
MODES;
BOOLS;
#undef ENTRYV
/* }}} */

static ErlNifResourceType *encoder_state_resource_type;
static ErlNifResourceType *decoder_state_resource_type;

#define ENTRYV(X, V) if (enif_compare(value, atom_##X) == 0) { *out = V; return 1; }
static int get_bool(ErlNifEnv *env, ERL_NIF_TERM value, unsigned int *out) {
  BOOLS;
  return 0;
}

static int get_mode(ErlNifEnv *env, ERL_NIF_TERM value, BrotliEncoderMode *out) {
  MODES;
  return 0;
}

static int get_encoder_parameter(ErlNifEnv *env, ERL_NIF_TERM value, BrotliEncoderParameter *out) {
  PARAMS;
  return 0;
}

static int get_encoder_option(ErlNifEnv *env, ERL_NIF_TERM value, BrotliEncoderOperation *out) {
  OPERATIONS;
  return 0;
}
#undef ENTRYV

#define BADARG       enif_make_badarg(env)
#define EBOOL(value) ((value) ? atom_true : atom_false)
#define T_OK2(value) enif_make_tuple2(env, atom_ok, value)

#define assert_argc(N) if (argc != N) { return BADARG; }

/* ALLOCATORS {{{ */
static void* brotli_alloc(void *opaque, size_t size) { return enif_alloc(size); }
static void  brotli_free (void *opaque, void  *addr) { return enif_free (addr); }
/* }}} */

/* ENCODER {{{ */
static ERL_NIF_TERM brotli_encoder_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(0);

  BrotliEncoderState *state = BrotliEncoderCreateInstance(brotli_alloc, brotli_free, NULL);
  BrotliEncoderState **resource = enif_alloc_resource(encoder_state_resource_type, sizeof(state));

  *resource = state;

  ERL_NIF_TERM ret = enif_make_resource(env, resource);
  enif_release_resource(resource);

  return ret;
}

static void brotli_encoder_dtor(ErlNifEnv *env, void *obj) {
  BrotliEncoderState **state = (BrotliEncoderState **)obj;
  BrotliEncoderDestroyInstance(*state);
}

static ERL_NIF_TERM brotli_encoder_set_parameter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(3);
  BrotliEncoderState **state;
  BrotliEncoderParameter param;
  unsigned int value;

  if (!enif_get_resource(env, argv[0], encoder_state_resource_type, (void **)&state)) {
    return BADARG;
  }

  if (!get_encoder_parameter(env, argv[1], &param)) {
    return BADARG;
  }

  switch (param) {
    case BROTLI_PARAM_MODE:
      if (!get_mode(env, argv[2], &value)) { return BADARG; }
      break;
    case BROTLI_PARAM_DISABLE_LITERAL_CONTEXT_MODELING:
      if (!get_bool(env, argv[2], &value)) { return BADARG; }
      value = !value;
      break;
    case BROTLI_PARAM_LARGE_WINDOW:
      if (!get_bool(env, argv[2], &value)) { return BADARG; }
      break;
    default:
      if (!enif_get_uint(env, argv[2], &value)) { return BADARG; }
  }

  return EBOOL(BrotliEncoderSetParameter(*state, param, value));
}

static ERL_NIF_TERM brotli_encoder_compress_stream(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(3);

  BrotliEncoderState **state;
  BrotliEncoderOperation op;
  ErlNifBinary input;

  if (!enif_get_resource(env, argv[0], encoder_state_resource_type, (void **)&state)) {
    return BADARG;
  }

  if (!get_encoder_option(env, argv[1], &op)) {
    return BADARG;
  }

  if (!enif_inspect_iolist_as_binary(env, argv[2], &input)) {
    return BADARG;
  }

  size_t available_out = 0;
  size_t available_in = input.size;
  const uint8_t *next_in = input.data;

  return EBOOL(BrotliEncoderCompressStream(
    *state, op,
    &available_in, &next_in,
    &available_out, NULL,
    NULL
  ));
}

static ERL_NIF_TERM brotli_encoder_has_more_output(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(1);
  BrotliEncoderState **state;

  if (!enif_get_resource(env, argv[0], encoder_state_resource_type, (void **)&state)) {
    return BADARG;
  }

  return EBOOL(BrotliEncoderHasMoreOutput(*state));
}

static ERL_NIF_TERM brotli_encoder_is_finished(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(1);
  BrotliEncoderState **state;

  if (!enif_get_resource(env, argv[0], encoder_state_resource_type, (void **)&state)) {
    return BADARG;
  }

  return EBOOL(BrotliEncoderIsFinished(*state));
}

static ERL_NIF_TERM brotli_encoder_take_output(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(1);
  BrotliEncoderState **state;
  ErlNifBinary output;

  if (!enif_get_resource(env, argv[0], encoder_state_resource_type, (void **)&state)) {
    return BADARG;
  }

  size_t size = 0;
  const uint8_t *data = BrotliEncoderTakeOutput(*state, &size);

  enif_alloc_binary(size, &output);

  memcpy(output.data, data, output.size);

  return enif_make_binary(env, &output);
}
/* }}} */

/* DECODER {{{ */
static ERL_NIF_TERM brotli_decoder_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(0);

  BrotliDecoderState *state = BrotliDecoderCreateInstance(brotli_alloc, brotli_free, NULL);
  BrotliDecoderState **resource = enif_alloc_resource(decoder_state_resource_type, sizeof(state));

  *resource = state;

  ERL_NIF_TERM ret = enif_make_resource(env, resource);
  enif_release_resource(resource);

  return ret;
}

static void brotli_decoder_dtor(ErlNifEnv *env, void *obj) {
  BrotliDecoderState **state = (BrotliDecoderState **)obj;
  BrotliDecoderDestroyInstance(*state);
}

static ERL_NIF_TERM brotli_decoder_decompress_stream(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(2);

  BrotliDecoderState **state;
  ErlNifBinary input;

  if (!enif_get_resource(env, argv[0], decoder_state_resource_type, (void **)&state)) {
    return BADARG;
  }

  if (!enif_inspect_iolist_as_binary(env, argv[1], &input)) {
    return BADARG;
  }

  size_t available_out = 0;
  size_t available_in = input.size;
  const uint8_t *next_in = input.data;

  BrotliDecoderResult result = BrotliDecoderDecompressStream(
    *state,
    &available_in, &next_in,
    &available_out, NULL,
    NULL
  );

  switch (result) {
    case BROTLI_DECODER_RESULT_SUCCESS:
    case BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT: return atom_ok;

    case BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT: return atom_more;

    default: return atom_error;
  }
}

static ERL_NIF_TERM brotli_decoder_is_used(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(1);
  BrotliDecoderState **state;

  if (!enif_get_resource(env, argv[0], decoder_state_resource_type, (void **)&state)) {
    return BADARG;
  }

  return EBOOL(BrotliDecoderIsUsed(*state));
}

static ERL_NIF_TERM brotli_decoder_has_more_output(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(1);
  BrotliDecoderState **state;

  if (!enif_get_resource(env, argv[0], decoder_state_resource_type, (void **)&state)) {
    return BADARG;
  }

  return EBOOL(BrotliDecoderHasMoreOutput(*state));
}

static ERL_NIF_TERM brotli_decoder_is_finished(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(1);
  BrotliDecoderState **state;

  if (!enif_get_resource(env, argv[0], decoder_state_resource_type, (void **)&state)) {
    return BADARG;
  }

  return EBOOL(BrotliDecoderIsFinished(*state));
}

static ERL_NIF_TERM brotli_decoder_take_output(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(1);
  BrotliDecoderState **state;
  ErlNifBinary output;

  if (!enif_get_resource(env, argv[0], decoder_state_resource_type, (void **)&state)) {
    return BADARG;
  }

  size_t size = 0;
  const uint8_t *data = BrotliDecoderTakeOutput(*state, &size);

  enif_alloc_binary(size, &output);

  memcpy(output.data, data, output.size);

  return enif_make_binary(env, &output);
}

// XXX: Maybe we should provide atom return there
static ERL_NIF_TERM brotli_decoder_error_description(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(1);
  BrotliDecoderState **state;

  if (!enif_get_resource(env, argv[0], decoder_state_resource_type, (void **)&state)) {
    return BADARG;
  }

  BrotliDecoderErrorCode error = BrotliDecoderGetErrorCode(*state);
  const char *string = BrotliDecoderErrorString(error);

  return enif_make_string(env, string, ERL_NIF_LATIN1);
}
/* }}} */

/* UTILS {{{ */
static ERL_NIF_TERM brotli_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(0);

  // Cache library version
  ERL_NIF_TERM major = enif_make_int(env, BROTLI_VERSION >> 24);
  ERL_NIF_TERM minor = enif_make_int(env, (BROTLI_VERSION >> 12) & 0xFFF);
  ERL_NIF_TERM patch = enif_make_int(env, BROTLI_VERSION & 0xFFF);

  return enif_make_tuple3(env, major, minor, patch);
}

static ERL_NIF_TERM brotli_max_compressed_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  assert_argc(1);
  size_t input_size;

  if (!enif_get_uint64(env, argv[0], &input_size)) { return BADARG; }

  return enif_make_uint64(env, BrotliEncoderMaxCompressedSize(input_size));
}
/* }}} */

/* NIF INITALIZATION {{{ */
static int brotli_init(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  // Create all atoms
  #define ENTRY(X) atom_##X = enif_make_atom(env, #X);
  ATOMS;
  #undef ENTRY
  #define ENTRYV(X, V) atom_##X = enif_make_atom(env, #X);
  PARAMS;
  OPERATIONS;
  MODES;
  BOOLS;
  #undef ENTRYV

  // Initialize resource types
  encoder_state_resource_type = enif_open_resource_type(
    env, NULL, "encoder_state", brotli_encoder_dtor, ERL_NIF_RT_CREATE, NULL
  );
  decoder_state_resource_type = enif_open_resource_type(
    env, NULL, "decoder_state", brotli_decoder_dtor, ERL_NIF_RT_CREATE, NULL
  );

  return 0;
}

static int brotli_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info) {
  return 0;
}

static ErlNifFunc brotli_exports[] = {
  /* Encoder functions */
  {"encoder_create", 0, brotli_encoder_create},
  {"encoder_set_parameter", 3, brotli_encoder_set_parameter},
  {"encoder_has_more_output", 1, brotli_encoder_has_more_output},
  {"encoder_is_finished", 1, brotli_encoder_is_finished},
  {"encoder_compress_stream", 3, brotli_encoder_compress_stream, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"encoder_take_output", 1, brotli_encoder_take_output, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  /* Decoder functions */
  {"decoder_create", 0, brotli_decoder_create},
  {"decoder_has_more_output", 1, brotli_decoder_has_more_output},
  {"decoder_is_finished", 1, brotli_decoder_is_finished},
  {"decoder_is_used", 1, brotli_decoder_is_used},
  {"decoder_error_description", 1, brotli_decoder_error_description},
  {"decoder_decompress_stream", 2, brotli_decoder_decompress_stream, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"decoder_take_output", 1, brotli_decoder_take_output, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  /* Utils */
  {"max_compressed_size", 1, brotli_max_compressed_size},
  {"version", 0, brotli_version},
};

ERL_NIF_INIT(brotli_nif, brotli_exports, brotli_init, NULL, brotli_upgrade, NULL)
/* }}} */
