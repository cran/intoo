"%$%" = function (object, name)
	attr (object, as.character (substitute (name) ) )

"%$%<-" = function (object, name, value)
  "attr<-" (object, as.character (substitute (name) ), value)

object.info = function (object,
	print.value=TRUE, print.value.length=TRUE, print.long.value=FALSE,
	print.class=TRUE, print.class.length=print.value.length, print.long.class=print.long.value,
	print.environment=FALSE,
	print.attributes=TRUE, print.many.attributes=TRUE,
	print.attribute.names=TRUE, print.attribute.classes=TRUE, print.attribute.lengths=print.value.length,
	print.attribute.values=TRUE, print.long.attribute.values=print.long.value,
	n=8L)
{	if (print.value)
	{	cat ("value")
		dims = dim (object)
		if (print.value.length)
		{	if (is.null (dims) )
				dims = length (object)
			cat (",", paste (dims, collapse=" * ") )
		}
		cat ("\n")
		object2 = object
		attributes (object2) = NULL
		if (is.vector (object2) )
			object2 %$% dim = dims
		if (is.function (object) )
			environment (object2) = .GlobalEnv
		if (!print.long.value)
			.print.head (object2, n)
		else
			print (object2)
	}

	if (print.class)
	{	cat ("class")
		c = class (object)
		if (print.class.length)
			cat (",", length (c) )
		cat ("\n")
		nc = length (c)
		if (!print.long.class && nc > n)
			c = c [1:n]	
		print (c)
	}

	e = environment (object)
	if (print.environment && !is.null (e) )
		print (e)

	if (print.attributes)
	{	objattrs = attributes (object)
		if (!is.null (objattrs) )
		{	objattrs = .remove.special.attributes (objattrs)
			
			nattrs = length (objattrs)
			if (!print.many.attributes && nattrs > n)
			{	nattrs = n
				objattrs = objattrs [1:n]
			}

			if (nattrs > 0)
			{	objattrs.names = names (objattrs)
				for (i in 1:nattrs)
				{	attr = objattrs [[i]]
					if (print.attribute.names)
					{	cat ("%$%", objattrs.names [i], sep="")
						if (print.attribute.classes)
							cat (",", class (attr)[1])
						if (print.attribute.lengths)
						{	dims = dim (attr)
							if (is.null (dims) )
								dims = length (attr)
							cat (",", paste (dims, collapse=" * ") )
							objattrs2 = attributes (attr)
							if (!is.null (objattrs2) )
								objattrs2 = .remove.special.attributes (objattrs2)
							n.attr.attrs = length (objattrs2)
							if (n.attr.attrs > 0)
								cat (" (", n.attr.attrs, ")", sep="")
						}
						cat ("\n")
					}
					if (print.attribute.values)
					{	dims = dim (attr)
						attributes (attr) = NULL
						if (is.vector (attr) )
							attr %$% dim = dims
						if (!print.long.attribute.values)
							.print.head (attr, n)
						else
							print (attr)
					}
				}
			}
		}
	}
}

.remove.special.attributes = function (objattrs)
{	objattrs$class = NULL
	objattrs$dim = NULL
	objattrs$srcref = NULL
	objattrs$.Environment = NULL
	objattrs
}

.print.head = function (object, n)
{	h = head (object, n)
	if (class (h) == "noquote")
		for (line in h)
				cat (line, "\n")
	else
		print (h)
}


