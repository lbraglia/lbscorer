R = R --quiet --vanilla

all: test
	mv sf12tests.Rout sf12tests.Rout.save
	mv sf36tests.Rout sf36tests.Rout.save

test:
	${R} < sf12tests.R > sf12tests.Rout
	${R} < sf36tests.R > sf36tests.Rout
