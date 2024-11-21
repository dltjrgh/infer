(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Map : Stdlib.Map.S with type key = string

module Set : Stdlib.Set.S with type elt = string

module Hash : Stdlib.Hashtbl.S with type key = string