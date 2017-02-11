# makefile for haskell players by Shuji Kinoshita since 2017-01-07

# When you add new player...
#  1. modify this file
#  2. modify ../players.txt

HASKFLAGS= -Wall -O2 
HASK= ghc

# TARGET= hogePlayer piyoPlayer
TARGET= hogePlayer

all: ${TARGET}

# This would be changed. (compile all players at once?)
hogePlayer: hogePlayer.hs
	${HASK} ${HASKFLAGS} $^
	mv $@ ../

# piyoPlayer: piyoPlayer.hs
#	${HASK} ${HASKFLAGS} $^
#	mv $@ ../

submit: hogePlayer.hs
	${HASK} ${HASKFLAGS} $^

clean:
	rm -f ${TARGET}
	rm -f *.o *.hi
	rm -rf *~ */*~ \#*\#
