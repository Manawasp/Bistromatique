## kyndt clovis
## start 2013/04/11
## name : Bistro
## lang : Haskell

NAME 	= bistro

SRC	= main.hs

RM 	= rm -f

all:	$(NAME)
	
$(NAME):
	ghc --make $(SRC) -o $(NAME)

clean:
	$(RM) *.hi *.o

fclean: clean
	$(RM) $(NAME)

re: fclean all
