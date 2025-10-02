(in-package :STUMPWM)

(setf *search-engines* nil)
(define-search-engines
    `(
      ("d" "ddg" "https://duckduckgo.com/lite/?q=~A")
      ("w" "wiki" "https://en.wikipedia.org/w/index.php?search=~A")
      ("y" "youtube" "https://www.youtube.com/results?search_query=~A")
      ("s" "soverflow" "http://stackoverflow.com/search?q=~A")
      ;; ("b" "bingimg" "http://www.bing.com/images/search?q=~A")
      ("b" "bingimg" "https://www.google.com/search?tbm=isch&q=~A")
      ("e" "ebay" "http://www.ebay.com/sch/i.html?&_nkw=~A")
      ("l" "localhost" "http://localhost:~A")
      ("G" "github" "https://github.com/search?utf8=%E2%9C%93&q=~A")
      ("p" "google-maps" "https://www.google.com/maps/search/~A")
      ("r" "realtor" "https://www.realtor.com/realestateandhomes-search/~A")
      ;; ("r" "word-reference" "http://www.wordreference.com/es/translation.asp?tranword=~A")
      ("B" "baidu" "https://www.baidu.com/s?wd=~A")
      ("R" "spanishdict" "http://www.spanishdict.com/examples/~A")
      ("L" "linguee" "https://www.linguee.com/english-spanish/search?source=auto&query=~A")
      ("g" "google" "https://www.google.com/search?num=20&q=~A")
      ("m" "music" "https://music.erjoalgo.com/by-lyrics?lucky=true&lyrics=~A")
      ("u" "youtube-music" "https://music.youtube.com/search?q=~A")
      ("m" "mvnrepository" "http://mvnrepository.com/search?q=~A")
      ;; ("W" "Who" "https://moma.corp.google.com/person/~A")
      ;; ("W" "walmart" "https://moma.corp.google.com/person/~A")
      ("Y" "Yaqs" "https://yaqs.googleplex.com/eng?query=~A")
      ("c" "codesearch" "https://cs.corp.google.com/?q=~A")
      ("t" "contacts" "http://localhost:1959/contacts")
      ("z" "shopping"
           ("https://www.amazon.com/s?k=~A"
            "https://www.temu.com/search_result.html?search_key=~A"
            "https://www.walmart.com/search?q=~A"
            "https://www.aliexpress.com/wholesale?SearchText=~A"
            "https://www.facebook.com/marketplace/orlando/search/?query=~A"))
      ("f" "whole-foods" "https://www.amazon.com/s?i=wholefoods&k=~A")
      ("h" "home-depot" "https://www.homedepot.com/s/~A")
      ("H" "lowes" "https://www.lowes.com/search?searchTerm=~A")
      ("a" "address" "~A")
      ("3" "3d-printables"
           ("https://www.yeggi.com/q/~A"
            "https://www.printables.com/search/models?q=~A"
            "https://www.thingiverse.com/search?q=~A&page=1&type=things&sort=relevant"
            "https://sketchfab.com/search?q=~A&type=models"
            "https://thangs.com/search/~A"))
      ("o" "orders" ("https://www.amazon.com/gp/your-account/order-history/search=~A"))
      ("i" "ikea" "https://www.ikea.com/us/en/search/?q=~A")))
