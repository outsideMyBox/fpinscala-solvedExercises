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
    
# To run the tests in sbt
    > sbt
    > ~testOnly <test>  // e.g. ~testOnly fpinscala.gettingstarted.GettingStartedSpec
    
Use your favorite editor to edit the exercise file. The test will run each time you save it.

# To modify/correct/develop your own tests.

By default, the tests are run against the module 'exercises', which doesn't contain any solution for this repository. If you want to modify or create new tests you can run them against
the existing solutions by running :

	> sbt -Dtest=answers
	
before launching the tests or creating the eclipse project.

	
The original readme can be found [here](https://github.com/fpinscala/fpinscala).
	

