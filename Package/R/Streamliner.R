# Many of these functions condense repetitive actions into function calls


#' @title Extract a File Name from a File Path 
#' 
#' @description
#' This function is a shortcut for applying [stringr::str_remove()] to remove 
#' all folders from a file path. For example, given a path like 
#' "C:/Users/.../example.txt" or "C:\\Users\\...\\example.txt", this function 
#' can return "example.txt". 
#' 
#' @details
#' 
#' This function relies on the default greedy behavior of pattern matching with 
#' [stringr::str_remove()]. The regular expression in `extract_filename` matches 
#' with everything in a string from its beginning to a backslash or forward slash. 
#' With a [greedy approach](https://stackoverflow.com/questions/2301285/what-do-lazy-and-greedy-mean-in-the-context-of-regular-expressions),
#' this will match everything in a file path until the final slash. The end result
#' of [stringr::str_remove()] will be the remnants of the string after that slash; 
#' this should be just the filename. 
#' 
#' @usage extract_file(path) 
#' 
#' @param path A [character] vector containing one or more file paths.
#' 
#' @returns A [character] [vector] with the same size as `path`.
#' 
#' @export
#' 
#' @examples
#' # Given a vector of paths, extract the filename portion
#' paths <- c("test.txt", "example/test.txt", "example_2\\test.txt")
#' 
#' # The function works with either a single string or multiple elements
#' extract_filename(paths[1])
#' 
#' extract_filename(paths)
extract_filename <- function (path) {
  
  # Extract a filename from a file path
  
  # (e.g., getting "example.txt" from "C:/Users/.../example.txt")
  
  return(path |>
           stringr::str_remove("^.+[/\\\\]"))
  
  # This regular expression removes everything from the beginning of the string
  # to a forward slash or backslash 
  # (since the search is greedy by default, it will stop at the final slash)
  
}


#' @title Find a Match in a Vector
#' 
#' @description
#' This function is a wrapper for the process of using [grep()] to find matches
#' in a vector. It includes an optional error handling procedure related to the 
#' number of matches found for a pattern. 
#' 
#' @details
#' 
#' `find_matches` helps find elements in a vector that satisfy a specified pattern 
#' requirement. Just like [grep()], it can return either a numeric vector of indices 
#' or a character vector with strings that match `pattern`. 
#' 
#' Please note that, aside from `ignore.case`, `fixed`, and `value`, this function 
#' relies on the default values of optional parameters in [grep()]. 
#' 
#' `minMatches` and `maxMatches` can help validate the results. With their default 
#' values, a `pattern` should result in exactly one match in `x`. If not, the function
#' outputs an error message. To disable the error handling procedure, consider 
#' setting `minMatches` and `maxMatches` to `-Inf` and `Inf`, respectively. 
#' 
#' `filePath` is an optional argument that only affects error messages. If `x`
#' originated from a file, the path to that file can be specified here. Then, if 
#' an error occurs, the output message will include this string. This can help 
#' with debugging. 
#' 
#' @usage find_matches(x, pattern, minMatches = 1, maxMatches = 1, 
#'              returnIndex = TRUE, filePath = NULL, 
#'              ignore.case = FALSE, fixed = FALSE) 
#' 
#' @param x A [character] vector that will be searched for matches.
#' 
#' @param pattern A [character] string that contains a regular expression. This
#' will be applied to `x` to search for matches.
#' 
#' @param minMatches An [integer] that identifies the minimum number of acceptable
#' matches that can be found by `pattern` in `x`.
#' 
#' @param maxMatches An [integer] that identifies the maximum number of acceptable
#' matches that can be found by `pattern` in `x`.
#' 
#' @param returnIndex A [logical] value that toggles whether a [numeric] or 
#' [character] vector is returned by the function. If TRUE (the default), matching elements' 
#' indices will be returned by the function. Otherwise, the function will return 
#' the actual elements within `x` that match `pattern`. (This is the complement of 
#' the `value` argument in [grep()])
#' 
#' @param filePath An optional `character` string (with default value `NULL`). This should 
#' be the path to the file that is the source of `x`. See 'Details' for more information. 
#' 
#' @param ignore.case A [logical] value that affects case sensitivity in the pattern 
#' matching algorithm. Exactly like [grep()], this argument is FALSE by default, 
#' causing `pattern` to be case sensitive in its search (e.g., "A" and "a" are 
#' treated differently). If this argument is TRUE, case is ignored during matching 
#' (e.g., "A" and "a" are equivalent). 
#' 
#' @param fixed A [logical] value that determines whether `pattern` is treated like 
#' a regular expression. Just like [grep()], this parameter is FALSE by default, 
#' and `pattern` is applied in matching as a regular expression. Specify `TRUE` if 
#' literal matches are desired (when TRUE, `fixed` can override other arguments). 
#' 
#' @returns Either a [character] or [integer] [vector]. Empty vectors are possible.
#' 
#' @export
#' 
#' @examples
#' # Given a vector of strings, find the index or indices that match a pattern
#' find_matches(month.abb, "o")
#' 
#' # To return the actual strings instead of their indices, use 'returnIndex'
#' find_matches(month.abb, "D", returnIndex = FALSE)
#' 
#' # Adjust expectations for results using 'minMatches' and 'maxMatches'
#' find_matches(month.abb, "e", minMatches = 3, maxMatches = 3)
#' 
#' # Disable error-checking entirely by setting 'minMatches' and 'maxMatches' to extreme values
#' find_matches(month.abb, "z", minMatches = -Inf, maxMatches = Inf)
find_matches <- function (x, pattern, minMatches = 1, maxMatches = 1, 
                         returnIndex = TRUE, filePath = NULL, 
                         ignore.case = FALSE, fixed = FALSE) {
  
  
  # Given a regular expression pattern, search a character vector for matches
  
  # If the number of matches does not match expectations, throw an error
  
  # Otherwise, return either the index of the match(es) or the values themselves
  # (based on the value of 'returnIndex')
  
  
  # First, apply `grep` to get the number of matches in 'x'
  matchIndex <- grep(pattern = pattern, x = x, value = !returnIndex, 
                     ignore.case = ignore.case, fixed = fixed)
  
  # 'ignore.case' and 'fixed' are the exact same arguments as their `grep` counterparts
  
  # When 'returnIndex' is TRUE, the 'value' argument is set to FALSE (returning index values)
  # When 'returnIndex' is FALSE, 'value' is TRUE, returning the matching string(s) instead
  
  
  # Next, check if the number of results is as expected
  
  
  # Make sure 'minMatches' and 'maxMatches' have reasonable values
  if (minMatches > maxMatches) {
    paste0("Argument 'minMatches' is greater than argument 'maxMatches'") |>
      stop_script()
  }
  
  
  # Check if 'matchIndex' has fewer matches than 'minMatches'
  if (length(matchIndex) < minMatches) {
    
    paste0("Expected ", minMatches, " match", 
           dplyr::if_else(minMatches != 1, "es", ""),
           ", but \"", pattern, "\" had only ", length(matchIndex), 
           " match", dplyr::if_else(length(matchIndex) != 1, "es", ""), ".", 
           dplyr::if_else(is.null(filePath), 
                   "",
                   paste0("\n\nPlease investigate \"", filePath, "\""))) |>
      stop_script()
    
  }
  
  
  # Alternatively, check if the length of 'matchIndex' exceeds 'maxMatches'
  if (length(matchIndex) > maxMatches) {
    
    paste0("Expected at most ", maxMatches, " match", 
           dplyr::if_else(maxMatches > 1, "es", ""),
           ", but \"", pattern, "\" had ", length(matchIndex), 
           " match", dplyr::if_else(length(matchIndex) > 1, "es", ""), " instead.",
           dplyr::if_else(is.null(filePath), 
                   "",
                   paste0("\n\nPlease investigate \"", filePath, "\""))) |>
      stop_script()
    
  }
  
  
  # Return 'matchIndex' if there are no issues
  return(matchIndex)
  
}


