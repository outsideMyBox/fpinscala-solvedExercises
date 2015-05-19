# About this fork

This fork adds unit tests to the exercises. It is still under development, the unit tests have been added up to the chapter 7 so far.

The modifications to the original repository have been kept to minimal but in some cases the solution or the exercise statement
may be slightly different to be testable. For example, for some exercises the method signatures were added. If
for one exercise in the book the method signature is not present you may want to guess it before jumping to the code!

The new sub-module 'tests' contains, who would have guessed, the unit tests. 

# To set your environment:
    > git clone https://github.com/RheinMainScala/fpinscala.git
    > cd fpinscala

    
# To run the tests in eclipse
    > sbt eclipse
    
In eclipse import the existing projects (Import... / General-Existing Projects into Workspace).
Tick the option "Search for nested projects" and add at least the projects 'exercises' and 'tests'.

The project 'tests' contains the unit tests to run against your code in 'exercises'. Run them as "Scala JUnit Test".
The tests use the library [specs2](https://etorreborre.github.io/specs2/) and especially a feature called [Datatables](https://etorreborre.github.io/specs2/guide/SPECS2-3.6/org.specs2.guide.UseDatatables.html).
    
# To run the tests in sbt
    > sbt
    > ~testOnly <test class> // e.g. ~testOnly fpinscala.gettingstarted.GettingStartedSpec
                             //  or   ~testOnly *GettingStartedSpec (if there is only one GettingStartedSpec)
    
    // You can choose the tests to run by specifying a regular expression:
    > ~testOnly <test class> -- -ex "<regexp>" // e.g. testOnly *GettingStartedSpec -- -ex "Exercise 2.1"

    
Use your favorite editor to edit the exercise file. The test will run each time you save it.

# To modify/correct/develop your own tests.

By default, the tests are run against the module 'exercises', which doesn't contain any solution for this repository. If you want to modify or create new tests you can run them against
the existing solutions by running :

	> sbt -Dtest=answers
	
before launching the tests or creating the eclipse project.

	
The original readme can be found [here](https://github.com/fpinscala/fpinscala).
	

