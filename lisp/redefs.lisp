(defun completing-read-notrim (screen prompt completions &key (initial-input "") require-match)
  "Read a line of input through stumpwm and return it with TAB
completion. Completions can be a list, an fbound symbol, or a
function. If its an fbound symbol or a function then that function is
passed the substring to complete on and is expected to return a list
of matches. If require-match argument is non-nil then the input must
match with an element of the completions."
  (check-type completions (or list function symbol))
  (read-one-line screen prompt
                 :completions completions
                 :initial-input initial-input
                 :require-match require-match))
