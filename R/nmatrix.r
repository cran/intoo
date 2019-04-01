.val.names = function (x)
{	if (is.null (rownames (x) ) )
		rownames (x) = paste ("[", 1:nrow (x), ",]", sep="")
	if (is.null (colnames (x) ) )
		colnames (x) = paste ("[,", 1:ncol (x), "]", sep="")
	x
}

P.nmatrix = function (x, sms)
	EXTEND (LIST (x, sms), "P.nmatrix")

P.smatrix = function (x1, x2, y1, y2)
{	L = list (x1=as.integer (x1), x2=as.integer (x2), y1=as.integer (y1), y2=as.integer (y2) )
	EXTEND (L, "P.smatrix")
}

P.ht = function (x, n=4)
{	nr = nrow (x)
	nc = ncol (x)
	x = .val.names (x)
	n2 = 2 * n
	if (nr <= n2 && nc <= n2)
		x
	else if (nr <= n2 && nc > n2)
	{	x = x [, c (1:n, (1 + nc - n):nc), drop=FALSE]
		sm.l = P.smatrix (1, nr, 1, n)
		sm.r = P.smatrix (1, nr, n + 1, n2)
		P.nmatrix (x, list (sm.l, sm.r) )
	}
	else if (nr > n2 && nc <= n2)
	{	x = x [c (1:n, (1 + nr - n):nr),, drop=FALSE]
		sm.l = P.smatrix (1, n, 1, nc)
		sm.u = P.smatrix (n + 1, n2, 1, nc)
		P.nmatrix (x, list (sm.l, sm.u) )
	}
	else
	{	x = x [c (1:n, (1 + nr - n):nr), c (1:n, (1 + nc - n):nc)]
		sm.ul = P.smatrix (1, n, 1, n)
		sm.ur = P.smatrix (1, n, n + 1, n2)
		sm.ll = P.smatrix (n + 1, n2, 1, n)
		sm.lr = P.smatrix (n + 1, n2, n + 1, n2)
		P.nmatrix (x, list (sm.ul, sm.ur, sm.ll, sm.lr) )
	}
}

print.P.nmatrix = function (x, ...)
	print (format (x, ...), quote=FALSE)

print.P.smatrix = function (x, ...)
{	sm = x

	cat ("{x1=", sm$x1, ", x2=", sm$x2, ", y1=", sm$y1, ", y2=", sm$y2, "}\n", sep="")
}

head.P.nmatrix = function (x, n=6L, ...)
{	x = format (x, ...)
	head (x, n)
}

tail.P.nmatrix = function (x, n=6L, ...)
{	x = format (x, ...)
	tail (x, n)
}

format.P.nmatrix = function (x, reverse=FALSE, na.string="", ...)
{	nm = x

	x = nm$x
	I = is.na (x)
	sms = nm$sms

	x = format (x, ...)
	x = .val.names (x)
	x [I] = na.string
	if (reverse)
		sms = rev (sms)

	nr = nrow (x)
	nc = ncol (x)
	nr.2 = 1 + 2 * nr
	nc.2 = 1 + 2 * nc

	rnames = rep ("", nr.2)
	cnames = rep ("", nc.2)
	rnames [seq (2, 2 * nr, by=2)] = rownames (x)
	cnames [seq (2, 2 * nc, by=2)] = colnames (x)

	nchars = max (nchar (x) )
	hbar = rep ("-", nchars)
	hbar = paste (hbar, collapse="")

	y = matrix ("", nr.2, nc.2)
	for (i in 1:nr)
	{	for (j in 1:nc)
			y [2 * i, 2 * j] = x [i, j]
	}
	rownames (y) = rnames
	colnames (y) = cnames

	for (sm in sms)
	{	y [c (2 * sm$x1 - 1, 2 * sm$x2 + 1), c (2 * sm$y1 - 1, 2 * sm$y2 + 1)] = "+"
		y [c (2 * sm$x1 - 1, 2 * sm$x2 + 1), (2 * sm$y1):(2 * sm$y2)] = hbar
		y [(2 * sm$x1):(2 * sm$x2), c (2 * sm$y1 - 1, 2 * sm$y2 + 1)] = "|"
	}

	y = y [-nr.2,, drop=FALSE]
	if (nr > 1)
	{	for (i in seq (2 * nr - 1, 3, by=-2) )
		{	if (!any (y [i,] == "+") )
				y = y [-i,]
		}
	}
	y = y [,-nc.2, drop=FALSE]
	if (nc > 1)
	{	for (j in seq (2 * nc - 1, 3, by=-2) )
		{	if (!any (y [,j] == "+") )
				y = y [,-j]
		}
	}
	y = y [-1,, drop=FALSE]
	y = y [,-1, drop=FALSE]

	y
}

dim.P.nmatrix = function (x)
	dim (x$x)
