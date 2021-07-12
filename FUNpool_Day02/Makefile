##
## EPITECH PROJECT, 2021
## FUN pool Day02
## File description:
## makefile
##

NAME	=	doop

SRC		=	DoOp.hs

OBJ		=	$(SRC:.hs=.o)
HI		=	$(SRC:.hs=.hi)

CFLAGS	=	-Wall -Wextra

all		:
			ghc $(CFLAGS) -o $(NAME) $(SRC)

clean	:
			rm -f $(OBJ) $(HI)

fclean	:	clean
			rm -f $(NAME)

re		:	fclean all

.PHONY	:	all clean fclean re