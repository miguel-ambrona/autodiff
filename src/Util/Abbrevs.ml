open Core_kernel

module F = Format
module L = List
module S = String

let optional ~d v = Option.value ~default:d v

let fixme s = failwith ("FIXME: "^s)
