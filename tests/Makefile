R = R --quiet --vanilla

all: test
	mv sf12tests.Rout sf12tests.Rout.save

test:
	${R} < sf12tests.R > sf12tests.Rout
