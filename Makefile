##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## makefile for my-pgp :)
##

NAME = mypgp

all: $(NAME)

$(NAME):
	stack build --copy-bins . --local-bin-path .
	mv mypgp-exe $(NAME)

clean:
	stack clean

fclean:
	stack clean
	rm -f $(NAME)

re: fclean all