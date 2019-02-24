"%$%" = function (object, name)
	attr (object, as.character (substitute (name) ) )

"%$%<-" = function (object, name, value)
  "attr<-" (object, as.character (substitute (name) ), value)

.object.info = function (object, value, private.attributes, public.attributes, values, comments, n=6)
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
					dims = .dim (x)
					x.attributes = attributes (x)
					x.attributes = .remove.special.attributes (x.attributes)
					#print attribute dscription
					cat ("%$% ", names.attributes [i], ", ", c, ", ", dims, sep="")
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
	if (comments && !is.null (com) )
		cat (com, sep="\n")
}

object.model = function (object,
	value=FALSE, private.attributes=FALSE, public.attributes=TRUE, values=value, comments=FALSE,
	n=3)
	.object.info (object, value, private.attributes, public.attributes, values, comments, n)

object.summary = function (object,
	value=TRUE, private.attributes=FALSE, public.attributes=TRUE, values=value, comments=TRUE,
	n=6)
	.object.info (object, value, private.attributes, public.attributes, values, comments, n)

object.info = function (object, n=6)
	object.summary (object, n=n)

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
{	if (inherits (object, "function") )
		attributes (object) = NULL
	h = head (object, n)
	if (class (h) == "noquote")
		for (l in h)
				cat (l, "\n", sep="")
	else
		print (h)
}
