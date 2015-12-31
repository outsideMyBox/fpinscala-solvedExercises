See https://github.com/RheinMainScala/fpinscala
The tests use the library [specs2](https://etorreborre.github.io/specs2/) and especially a feature called [Datatables](https://etorreborre.github.io/specs2/guide/SPECS2-3.6/org.specs2.guide.UseDatatables.html).
    > ~testOnly <test class> // e.g. ~testOnly fpinscala.gettingstarted.GettingStartedSpec
                             //  or   ~testOnly *GettingStartedSpec (if there is only one GettingStartedSpec)
    
    // You can choose the tests to run by specifying a regular expression:
    > ~testOnly <test class> -- -ex "<regexp>" // e.g. testOnly *GettingStartedSpec -- -ex "Exercise 2.1"
