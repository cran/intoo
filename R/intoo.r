"%$%" = function (object, name)
	attr (object, as.character (substitute (name) ) )

"%$%<-" = function (object, name, value)
  "attr<-" (object, as.character (substitute (name) ), value)

 .LIST = function (..., start=1)
{	sc = as.list (sys.call (-1) )[-(1:start)]
	L = list (...)
	names.1 = names (sc)
	names.2 = as.character (sc)
	if (is.null (names.1) )
		names (L) = names.2
	else
	{	unnamed = (names.1 == "")
		names (L) = names.1
		names (L) [unnamed] = names.2 [unnamed]
	}
	L
}

LIST = function (...)
	.LIST (...)

EXTEND = function (object, class, ...)
{	cl = list (c (class, class (object) ) )
	a1 = attributes (object)
	a1$class = NULL
	a2 = .LIST (..., start=3)
	attributes (object) = c (class=cl, a1, a2)
	object
}

THIS = function ()
	sys.function (-1)

THAT = function ()
{	this = sys.function (-1)
	attributes (this)
}

THEN = function ()
{	this = sys.function (-1)
	environment (this)
}
