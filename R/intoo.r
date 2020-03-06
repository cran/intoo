#intoo: Minimal Language-Like Extensions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

"%$%" = function (object, name)
	attr (object, as.character (substitute (name) ) )

"%$%<-" = function (object, name, value)
	"attr<-" (object, as.character (substitute (name) ), value)

"%@%" = function (object, name)
	get (as.character (substitute (name) ), envir = environment (object) )

"%@%<-" = function (object, name, value)
{	assign (as.character (substitute (name) ), value, envir = environment (object) )
	object
}

.LIST = function (..., .list.start.at=1)
{	sc = as.list (sys.call (-1) )[-(1:.list.start.at)]
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
{	cl = base::class (object)
	if (! missing (class) && ! is.null (class) )
		cl = c (class, cl)
	cl= list (cl)
	a1 = attributes (object)
	a1$class = NULL
	a2 = .LIST (..., .list.start.at=3)
	attributes (object) = c (class=cl, a1, a2)
	object
}

UNPACK = function (x)
{	list2env (x, parent.frame (1) )
	invisible (NULL)
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
