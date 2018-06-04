# Hit

Hit - Haskell Git. This is a simple implementation of some basic Git commands in Haskell.

## Motivation
The project is a university project written as a project to **Functional programming in Haskell** course.
I always admire Git version control system so I decided to get my inspiration from it. The system has been
developed to behave as close as possible to original Git, but still some simplifications was made.

## Available commands
For now the following commands are implemented:
- **hit init** - Initializes empty Hit repository
- **hit status** - Gets the status of repository, in particular list of changes made in working directory
- **hit commit** - Commits the working directory state
- **hit newbranch** - Creates a new branch
- **hit removebranch** - Removes existing branch
- **hit listbranch** - Lists existing branches
- **hit checkout** - Checkout to an existing branch
- **hit checkoutc** - Checkout to the given commit (puts repository in detached head mode)
- **hit diff** - Lists changes made to the given file from last commit
- **hit diffc** - Compares version of the same file in two commits
- **hit reset** - Resets changes made to a file in a working directory
- **hit resetall** - Resets all changes made in working directory
- **hit log** - Get a given number of commits from history
- **hit merge** - Merges the given branch into a current one.
- **hit getconfig** - Gets value from .hitconfig file saved at given key
- **hit setconfig** - Saves given key and value in .hitconfig file
- **hit listcommands** - Lists all available commands
- **hit help** - Displays help page for given command

Unfortunatelly there is no support for command options yet. Hopefully it will be added in next versions to make Hit more similar to actual Git 

## Built With

* [Stack](https://github.com/commercialhaskell/stack)

## Authors

* **Mateusz Bąkała** - [bakala12](https://github.com/bakala12)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Great thanks to [Git](https://github.com/git/git) - the most amaizing version control system in the world