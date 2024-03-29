#' Query k-gram frequency tables or dictionaries
#'
#' Return the frequency count of k-grams in a k-gram frequency table, or 
#' whether words are contained in a dictionary.
#'
#' @author Valerio Gherardi
#' @md
#'
#' @param object a \code{kgram_freqs} or \code{dictionary} class object.
#' @param x a character vector. A list of k-grams if \code{object} is of class 
#' \code{kgram_freqs}, a list of words if \code{object} is a \code{dictionary}.
#' @return an integer vector, containing k-gram counts of \code{x}, if 
#' \code{object} is a \code{kgram_freqs} class object, a logical vector if
#' \code{object} is a \code{dictionary}. Vectorized over \code{x}.
#' @details This generic has slightly different behaviors when querying 
#' for the presence of words in a dictionary and for k-gram counts 
#' in a frequency table respectively. 
#' For words, \code{query()} looks for exact matches between the input and the
#' dictionary entries. Queries of Begin-Of-Sentence (\code{BOS()}) and 
#' End-Of-Sentence (\code{EOS()}) tokens always return \code{TRUE}, and queries
#' of the Unknown-Word token return \code{FALSE} 
#' (see \link[kgrams]{special_tokens}).
#' 
#' On the other hand, queries of k-gram counts first perform a word level 
#' tokenization, so that anything separated by one or more space characters 
#' in the input is considered as a single word (thus, for instance queries of 
#' strings such as \code{"i love you"}, \code{" i love you"}), or  
#' \code{"i love you "}) all produce the same outcome). Moreover,
#' querying for any word outside the underlying dictionary returns the counts
#' corresponding to the Unknown-Word token (\code{UNK()}) (e.g., if 
#' the word \code{"prcsrn"} is outside the dictionary, querying 
#' \code{"i love prcsrn"} is the same as querying 
#' \code{paste("i love", UNK())}). Queries from k-grams of order \code{k > N}
#' will return \code{NA}.
#' 
#' A subsetting equivalent of query, with synthax \code{object[x]} is available 
#' (see the examples).
#' \code{query(object, x)}. The query of the empty string \code{""} returns the
#' total count of words, including the \code{EOS} and \code{UNK} tokens, but not
#' the \code{BOS} token.
#' 
#' See also the examples below.
#'    
#' @examples
#' # Querying a k-gram frequency table
#' f <- kgram_freqs("a a b a b b a b", N = 2)
#' query(f, c("a", "b")) # query single words
#' query(f, c("a b")) # query a 2-gram
#' identical(query(f, "c"), query(f, "d"))  # TRUE, both "c" and "d" are <UNK>
#' identical(query(f, UNK()), query(f, "c")) # TRUE
#' query(f, EOS()) # 1, since text is a single sentence
#' f[c("b b", "b")] # query with subsetting synthax 
#' f[""] # 9 (includes the EOS token)
#' 
#' # Querying a dictionary
#' d <- as_dictionary(c("a", "b"))
#' query(d, c("a", "b", "c")) # query some words
#' query(d, c(BOS(), EOS(), UNK())) # c(TRUE, TRUE, FALSE)
#' d["a"] # query with subsetting synthax
#' 
#' @name query

#' @rdname query
#' @export
query <- function(object, x) {
        assert_character_no_NA(x)
        UseMethod("query", object)
}
        

#' @rdname query
#' @export
query.kgram_freqs <- function(object, x) {
        attr(object, "cpp_obj")$query(x)
}

#' @export
`[.kgram_freqs` <- function(x, i) query(x, i)

#' @rdname query
#' @export
query.kgrams_dictionary <- function(object, x) {
        attr(object, "cpp_obj")$query(x)
}

#' @export
`[.kgrams_dictionary` <- function(x, i) query(x, i)
