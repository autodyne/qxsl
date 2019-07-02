<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="xml" indent="yes"/>
	<xsl:template match="/">
		<xsl:apply-templates select="ADX"/>
		<xsl:apply-templates select="list"/>
	</xsl:template>
	<!-- conversion from ADX to QXSL -->
	<xsl:template match="ADX">
		<list xmlns:adif="adif.org">
			<xsl:apply-templates select="RECORDS/RECORD"/>
		</list>
	</xsl:template>
	<!-- conversion from QXSL to ADX -->
	<xsl:template match="list">
		<ADX>
			<HEADER>
				<PROGRAMID>qxsl</PROGRAMID>
			</HEADER>
			<RECORDS>
				<xsl:apply-templates select="item"/>
			</RECORDS>
		</ADX>
	</xsl:template>
	<!-- conversion from ADX to QXSL -->
	<xsl:template match="RECORD">
		<item>
			<xsl:for-each select="*">
				<xsl:attribute name="adif:{name()}" namespace="adif.org">
					<xsl:value-of select="."/>
				</xsl:attribute>
			</xsl:for-each>
		</item>
	</xsl:template>
	<!-- conversion from QXSL to ADX -->
	<xsl:template match="item">
		<RECORD>
			<xsl:for-each select="@adif:*" xmlns:adif="adif.org">
				<xsl:element name="{local-name()}">
					<xsl:value-of select="."/>
				</xsl:element>
			</xsl:for-each>
		</RECORD>
	</xsl:template>
</xsl:stylesheet>
