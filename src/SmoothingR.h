/// @file   Dictionary.h 
/// @brief  Definition of Smoother classes 
/// @author Valerio Gherardi

#ifndef SMOOTHING_R_H
#define SMOOTHING_R_H

#include "Smoothing.h"
#include "kgramFreqsR.h"
#include <Rmath.h>
#include <Rcpp.h>
using namespace Rcpp;

// Not implemented. Sampling words using simple rejection method.
// N.B.: requires normalized probabilities.
//
// std::string sample_word_rej(std::string context) {
//         std::string res;
//         double best = 0, tmp;
//         std::string word;
//         while (true) {
//                 size_t n = R::runif(0, prob_.V_);
//                 word = prob_.f_.dictionary()->word(std::to_string(n));
//                 if (prob_(word, context) / R::runif(0, 1) > 1)
//                         return word;
//         }
// }

std::string sample_word_generic (Smoother * smoother, 
                                 std::string context, 
                                 double T = 1.0)
{
        std::string res;
        double best = 0, tmp;
        std::string word;
        // Sample word from P(word|context) using Gumbel-Max trick
        for (size_t i = 1; i <= smoother->V(); ++i) {
                word = smoother->word(std::to_string(i));
                tmp = smoother->operator()(word, context);
                tmp = std::pow(tmp, 1 / T);
                tmp /= R::rexp(1.);
                if (tmp > best) {
                        best = tmp;
                        res = word;
                }
        }
        // Separate iteration for EOS token
        tmp = smoother->operator()(EOS_TOK, context);
        tmp = std::pow(tmp, 1 / T);
        tmp /= R::rexp(1.);
        if (tmp > best)
                res = EOS_TOK;
        // N.B.: we forbid sampling the UNK token
        return res;
}

std::string sample_sentence_generic (Smoother * smoother, 
                                     size_t max_length, 
                                     double T = 1.0)
{
        std::string res = "", context = "";
        for (size_t i = 1; i < smoother->N(); ++i) {
                context += BOS_TOK + " ";
        }
        size_t n_words = 0;
        std::string new_word; size_t start = 0;
        while (n_words < max_length) {
                n_words++;
                new_word = sample_word_generic(smoother, context, T);
                if (new_word == EOS_TOK) 
                        return res + "<EOS>";
                res += new_word + " ";
                context += " " + new_word;
                start = context.find_first_not_of(" ");
                start = context.find_first_of(" ", start);
                context = context.substr(start + 1);
        }
        return res + "[...] (truncated output)";     
}

CharacterVector sample_generic (Smoother * smoother,
                                size_t n,
                                size_t max_length, 
                                double T = 1.0)
{
        CharacterVector res(n);
        for (size_t i = 0; i < n; ++i)
                res[i] = sample_sentence_generic(smoother, max_length, T);
        return res;
}

NumericVector probability_generic (Smoother * smoother,
                                   CharacterVector word,
                                   std::string context
) 
{
        size_t len = word.length();
        NumericVector res(len);
        std::string tmp;
        for (size_t i = 0; i < len; ++i) {
                tmp = word[i];
                res[i] = smoother->operator()(tmp, context);
                if (res[i] == -1) res[i] = NA_REAL;
        }
        return res;
}

NumericVector probability_generic(Smoother * smoother, 
                                  CharacterVector sentence) 
{
        size_t len = sentence.length();
        NumericVector res(len);
        std::string tmp;
        for (size_t i = 0; i < len; ++i) {
                tmp = sentence[i];
                res[i] = smoother->operator()(tmp).first;
                        if (res[i] == -1) res[i] = NA_REAL;
        }
        return res;
}

List log_prob_generic(Smoother * smoother, CharacterVector sentence) 
{
        size_t len = sentence.length();
        NumericVector log_prob(len);
        IntegerVector n_words(len);
        std::string tmp_sent;
        std::pair<double, size_t> tmp_res;
        for (size_t i = 0; i < len; ++i) {
                tmp_sent = sentence[i];
                tmp_res = smoother->operator()(tmp_sent, true);
                log_prob[i] = tmp_res.first;
                n_words[i] = tmp_res.second;
                if (std::isnan(tmp_res.first)) log_prob[i] = NA_REAL;
        }
        return List::create(_["log_prob"] = log_prob, _["n_words"] = n_words);
}

#endif //SMOOTHING_R_H
