##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## create pushswap_checker binary
##

NAME	=	pushswap_checker

SRC		=	src/main.hs		\
			src/commands.hs

TEST	=	tests/main.hs		\
			tests/commands.hs	\
			tests/unitTests.hs

OBJ		= 	$(SRC:.hs=.o)
HI		=	$(SRC:.hs=.hi)

TEST_OBJ=	$(TEST:.hs=.o)
TEST_HI	=	$(TEST:.hs=.hi)

CFLAGS	=	-Wall -Wextra

all		:
			ghc $(CFLAGS) -o $(NAME) $(SRC)

tests_run	:
				ghc $(CFLAGS) -o unit_tests $(TEST)

clean	:
			rm -f $(OBJ) $(HI) $(TEST_OBJ) $(TEST_HI)

fclean	:	clean
			rm -f $(NAME) unit_tests

re		:	fclean all

.PHONY	:	all clean fclean re
