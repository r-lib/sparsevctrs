.onLoad <- function(libname, pkgname) {
  .Call(ffi_init_library, ns_env())
}
