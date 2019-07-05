/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.table;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.time.ZoneId;
import java.util.List;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.xml.sax.SAXException;
import qxsl.model.Item;

/**
 * ADXサブセット書式で交信記録を直列化するフォーマットです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/27
 *
 */
public final class AdxsFormat extends BaseFormat {
	private final SchemaFactory schema;
	private final URL xsdURL;

	public AdxsFormat() {
		super("adxs");
		this.schema = SchemaFactory.newDefaultInstance();
		this.xsdURL = getClass().getResource("adxs.xsd");
	}

	@Override
	public boolean validate(InputStream strm) {
		try {
			Schema schema = this.schema.newSchema(this.xsdURL);
			schema.newValidator().validate(new StreamSource(strm));
			return true;
		} catch(SAXException | IOException ex) {
			return false;
		}
	}

	@Override
	public List<Item> decode(InputStream strm, ZoneId zone) throws IOException {
		final InputStream xslt = AdxsFormat.class.getResourceAsStream("adxs.xsl");
		final QxmlFormat qxml = new QxmlFormat();
		try {
			final TransformerFactory tf = TransformerFactory.newInstance();
			Transformer trans = tf.newTransformer(new StreamSource(xslt));
			final ByteArrayOutputStream buf = new ByteArrayOutputStream();
			trans.transform(new StreamSource(strm), new StreamResult(buf));
			return qxml.decode(new ByteArrayInputStream(buf.toByteArray()), zone);
		} catch(TransformerException ex) {
			throw new IOException(ex);
		}
	}

	@Override
	public void encode(OutputStream strm, List<Item> items) throws IOException {
		final InputStream xslt = AdxsFormat.class.getResourceAsStream("adxs.xsl");
		final QxmlFormat qxml = new QxmlFormat();
		try {
			final TransformerFactory tf = TransformerFactory.newInstance();
			Transformer trans = tf.newTransformer(new StreamSource(xslt));
			final ByteArrayOutputStream buf = new ByteArrayOutputStream();
			qxml.encode(buf, items);
			InputStream in = new ByteArrayInputStream(buf.toByteArray());
			trans.transform(new StreamSource(in), new StreamResult(strm));
		} catch(TransformerException ex) {
			throw new IOException(ex.getMessage(), ex);
		}
	}
}
