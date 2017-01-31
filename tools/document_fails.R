#------ devtools
#install_github("hadley/devtools") #‘1.12.0.9000’
pkg = "."

document <- function (pkg = ".", clean = NULL, roclets = NULL, reload = TRUE) {
    devtools:::check_suggested("roxygen2")
    if (!missing(clean)) {
        warning("`clean` is deprecated: roxygen2 now automatically cleans up",
            call. = FALSE)
    }
    if (!missing(reload)) {
        warning("`reload` is deprecated: code is now always reloaded",
            call. = FALSE)
    }
    pkg <- as.package(pkg)
    message("Updating ", pkg$package, " documentation")
    load_all(pkg)
    if (packageVersion("roxygen2") > "4.1.1") {
#        roclets <- roclets %||% roxygen2::load_options(pkg$path)$roclets
roclets <- roxygen2::load_options(pkg$path)$roclets
        roclets <- setdiff(roclets, "collate")
    }

    withr::with_envvar(r_env_vars(), roxygen2::roxygenise(pkg$path,
        roclets = roclets, load_code = pkgload::ns_env))
#> Warning: @details [SWSF_Spatial_Functions.R#1]: mismatched braces or quotes
#> Error: is.character(content) is not TRUE
    pkgload::dev_topic_index_reset(pkg$package)
    invisible()
}


#------ roxygen2
#install_github("klutometis/roxygen") # ‘5.0.1.9000’


package.dir <- "/Users/drschlaep/Dropbox (Personal)/Work_Stuff/2_Research/Software/GitHub_Projects/SoilWat_R_Wrapper"
roclets = c("namespace", "rd")
load_code = pkgload::ns_env
clean = FALSE

roxygenise <- function (package.dir = ".", roclets = NULL, load_code = source_package,
    clean = FALSE) {

    is_first <- roxygen2:::first_time(package.dir)
    if (is_first) {
        message("First time using roxygen2. Upgrading automatically...")
    }
    base_path <- normalizePath(package.dir)
    man_path <- file.path(base_path, "man")
    dir.create(man_path, recursive = TRUE, showWarnings = FALSE)
    roxygen2:::update_roxygen_version(base_path)
    options <- roxygen2:::load_options(base_path)
#    roclets <- roclets %||% options$roclets
    if ("collate" %in% roclets) {
        roxygen2:::update_collate(base_path)
        roclets <- setdiff(roclets, "collate")
    }
    if (length(roclets) == 0)
        return(invisible())
    roclets <- lapply(roclets, roclet_find)
    tags <- c(lapply(roclets, roclet_tags), list(list(include = tag_value)))
    registry <- unlist(tags, recursive = FALSE)
    parsed <- roxygen2:::parse_package(base_path, load_code, registry, options)
#> Warning: @details [SWSF_Spatial_Functions.R#1]: mismatched braces or quotes
    roc_out <- function(roc) {
        if (clean) {
            roclet_clean(roc, base_path)
        }
        results <- roclet_process(roc, parsed, base_path)
        roclet_output(roc, results, base_path, is_first = is_first)
    }
    invisible(unlist(lapply(roclets, roc_out)))
#> Error: is.character(content) is not TRUE
}


#---
roc <- roclets[[2]] #roclet_rd

roc_out <- function(roc) {
    if (clean) {
        roclet_clean(roc, base_path)
    }
    results <- roclet_process(roc, parsed, base_path)
#> Error: is.character(content) is not TRUE
    roclet_output(roc, results, base_path, is_first = is_first)
}


#---
x <- roclets[[2]] #roclet_rd
global_options = list()

roclet_process <- getS3method("roclet_process", "roclet_rd")
function (x, parsed, base_path, global_options = list()) {
    topics <- roxygen2:::RoxyTopics$new()
k <- 0
    for (block in parsed$blocks) {
k <- k + 1
print(k)
        if (length(block) == 0)
            next
        rd <- roxygen2:::block_to_rd(block, base_path, parsed$env, global_options)
# the following line fails for k = 13 with
# > Error: is.character(content) is not TRUE
        topics$add(rd)
    }
    topics_process_family(topics)
    topics_process_inherit(topics, parsed$env)
    topics$drop_invalid()
    topics_fix_params_order(topics)
    topics$topics
}


#######


roxygen2:::parse_package <- function (base_path, load_code) {
    env <- load_code(base_path)
# length(ls(env)) # 327
# the following line fails with
#  > Error in as.environment(where) : using 'as.environment(NULL)' is defunct
    parsed <- lapply(roxygen2:::package_files(base_path), roxygen2:::parse_file, env = env)
    blocks <- unlist(parsed, recursive = FALSE)
    list(env = env, blocks = blocks)
}

file <- roxygen2:::package_files(base_path)[1]

roxygen2:::parse_file <- function (file, env) {
    parsed <- parse(file = file, keep.source = TRUE)
    if (length(parsed) == 0)
        return()
    refs <- utils::getSrcref(parsed)
    comment_refs <- roxygen2:::comments(refs)
    extract <- function(call, ref, comment_ref) {
        preref <- roxygen2:::parse_preref(comment_ref, file)
        if (length(preref) == 0)
            return()
        preref$object <- roxygen2:::object_from_call(call, env, preref)
        preref$srcref <- list(filename = file, lloc = as.vector(ref))
        roxygen2:::add_defaults(preref)
    }
# the following line fails with
#> Error in print.function(x, useSource, ...) :
#>   non-function argument to .Internal(print.function(.))
    Map(extract, parsed, refs, comment_refs)
# equivalent to
    mapply(FUN = extract, parsed, refs, comment_refs, SIMPLIFY = FALSE)
}

k <- 1 # 1:8

call <- parsed[[k]]
ref <- refs[[k]]
comment_ref <- comment_refs[[k]]


    extract <- function(call, ref, comment_ref) {
        preref <- roxygen2:::parse_preref(comment_ref, file)
        if (length(preref) == 0)
            return()
# the following line fails with
#> Error in print.function(x, useSource, ...) :
#>   non-function argument to .Internal(print.function(.))
        preref$object <- roxygen2:::object_from_call(call, env, preref)
        preref$srcref <- list(filename = file, lloc = as.vector(ref))
        roxygen2:::add_defaults(preref)
    }


block <- preref

roxygen2:::object_from_call <- function (call, env, block) {
    if (is.null(call))
        return()
    if (is.character(call)) {
        value <- find_data(call, env)
        value <- standardise_obj(call, value, env, block)
        return(object(value, call))
    }
    if (!is.call(call))
        return()
    call <- roxygen2:::standardise_call(call, env)
    name <- as.character(call[[1]])
    if (length(name) > 1)
        return(NULL)
    parser <- roxygen2:::find_parser(name)
    if (is.null(parser))
        return(NULL)
# the following line fails with
#> Error in print.function(x, useSource, ...) :
#>   non-function argument to .Internal(print.function(.))
    parser(call, env, block)
}

parser <- function (call, env, block) {
    name <- as.character(call[[2]])
    if (length(name) > 1)
        return()
    if (!exists(name, env))
        return()
    value <- get(name, env)
    value <- roxygen2:::standardise_obj(name, value, env, block)
# the following line fails with
#> Error in print.function(x, useSource, ...) :
#>   non-function argument to .Internal(print.function(.))
    roxygen2:::object(value, name)
}

alias <- name
type <- roxygen2:::obj_type(value) #"function"

roxygen2:::object <- function (value, alias = NULL, type = obj_type(value)) {
    structure(list(
      alias = alias,
      value = value,
      methods = if (type == "rcclass") rc_methods(value)),
      class = c(type, "object"))
}


#------

