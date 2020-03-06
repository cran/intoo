#intoo: Minimal Language-Like Extensions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.object.info = function (object, value, private.attributes, public.attributes, values, comment, n)
{	#print object's description
	c = class (object) [1]
	dims = .dim (object)
	cat (c, ", ", paste (dims, collapse=" * "), "\n", sep="")

	#print object's value
	if (value)
		.print.head (object, n)

	#print attributes
	. = attributes (object)
	com = .$comment
	if (!is.null (.) )
		. = .remove.special.attributes (.)
	if (!is.null (.) )
	{	n.attributes = length (.)
		names.attributes = names (.)
		if (n.attributes > 0)
		{	for (i in 1:n.attributes)
			{	char.1 = substring (names.attributes [i], 1, 1)
				if (private.attributes && char.1 == "." || public.attributes && char.1 != ".")
				{	x = . [[i]]
					c = class (x) [1]
					if (c == "matrix")
						c = paste (mode (x), c)
					dims = .dim (x)
					x.attributes = attributes (x)
					x.attributes = .remove.special.attributes (x.attributes)
					#print attribute description
					cat ("%$% ", names.attributes [i], ", ", c, ", ", paste (dims, collapse=" * "), sep="")
					if (!is.null (x.attributes) )
					{	names.x.attributes = names (x.attributes)
						char.1 = substring (names.x.attributes, 1, 1)
						n.x.private = sum (char.1 == ".")
						n.x.attributes = length (x.attributes)
						n.x.public = n.x.attributes - n.x.private
						n.print = 0
						if (private.attributes && public.attributes)
							n.print = n.x.attributes
						else if (private.attributes)
							n.print = n.x.private
						else if (public.attributes)
							n.print = n.x.public
						#print number of subattributes
						if (n.print > 0)
							cat (", (", n.print, ")", sep="")
					}
					cat ("\n")
					#print attribute value
					if (values)
						.print.head (. [[i]], n)
				}
			}
		}
	}
	#print comments
	if (comment && !is.null (com) )
		cat (com, sep="\n")
}

object.model = function (object, ...,
	value=FALSE, private.attributes=TRUE, public.attributes=TRUE, attribute.values=value, comment=FALSE,
	n=3)
	.object.info (object, value, private.attributes, public.attributes, attribute.values, comment, n)

object.summary = function (object, ...,
	value=TRUE, private.attributes=FALSE, public.attributes=TRUE, attribute.values=value, comment=TRUE,
	n=6)
	.object.info (object, value, private.attributes, public.attributes, attribute.values, comment, n)

.remove.special.attributes = function (.)
{	.$class = NULL
	.$comment = NULL
	.$srcref = NULL
	.$.Environment = NULL
	.$dim = NULL
	.$names = NULL
	.$dimnames = NULL
	.$row.names = NULL

	.
}

.dim = function (object)
{	dims = dim (object)
	if (is.null (dims) )
	{	if (is.function (object) )
		{	b = body (object)
			if (class (b) == "{")
				dims = length (format (body (object) ) )
			else
				dims = 1
		}
		else
			dims = length (object)
	}
	dims
}

.print.head = function (object, n)
{	if (isS4 (object) )
	{	h = head (object, n)
		show (h)
	}
	else if (is.list (object) && !is.data.frame (object) )
		.print.head.list (object)
	else if (is.function (object) )
	{	attributes (object) = NULL
		h = head (object, n)
		for (l in h)
				cat (l, "\n", sep="")
	}
	else
	{	if (is.factor (object) )
			object = as.character (object)
		h = head (object, n)
		print (h, quote=FALSE)
	}
}

.print.head.list = function (object)
{	list.names = names (object)
	if (!is.null (list.names) )
	{	cat ("    %$% names\n")
		cat ("    (", paste (list.names, collapse=", "), ")\n", sep="")
	}
}
