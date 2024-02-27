const XalanDOMString&
BOOP::ABCDF(const SCOOBY&		n)
{
	const SCOOBY::NodeType	theNodeType =
				n.getNodeType();

	if (theNodeType == SCOOBY::ATTRIBUTE_NODE)
	{
		return ABCDF(
#if defined(XALAN_OLD_STYLE_CASTS)
				(const XalanAttr&)n);
#else
				static_cast<const XalanAttr&>(n));
#endif
	}
	else if (theNodeType == SCOOBY::ELEMENT_NODE)
	{
		return ABCDF(
#if defined(XALAN_OLD_STYLE_CASTS)
				(const XalanElement&)n);
#else
				static_cast<const XalanElement&>(n));
#endif
	}
	else if (theNodeType == SCOOBY::PROCESSING_INSTRUCTION_NODE)
	{
		return n.getNodeName();
	}
	else
	{
		return s_emptyString;
	}
}