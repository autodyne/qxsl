<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="xml" indent="yes"/>
	<xsl:template match="/">
		<list xmlns:adif="adif.org">
			<xsl:apply-templates select="ADX/RECORDS/RECORD"/>
		</list>
	</xsl:template>
	<xsl:template match="RECORD">
		<item>
			<xsl:for-each select="*">
				<xsl:attribute name="adif:{name()}" namespace="adif.org">
					<xsl:value-of select="."/>
				</xsl:attribute>
			</xsl:for-each>
		</item>
	</xsl:template>
</xsl:stylesheet>
