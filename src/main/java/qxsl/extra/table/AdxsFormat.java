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

import static javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI;

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
	private final Transformer former;
	private final Schema schema;

	/**
	 * {@link Schema}の定義を読み込んでフォーマットを初期化します。
	 *
	 * @throws IOException XSLTの定義を読み込めない場合
	 * @throws SAXException ADXサブセット文書のスキーマに問題がある場合
	 * @throws TransformerException XSLT変換器を構築できない場合
	 */
	public AdxsFormat() throws IOException, SAXException, TransformerException {
		super("adxs");
		TransformerFactory tf = TransformerFactory.newInstance();
		final URL xsl = AdxsFormat.class.getResource("adxs.xsl");
		this.former = tf.newTransformer(new StreamSource(xsl.openStream()));
		SchemaFactory sf = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI);
		this.schema = sf.newSchema(AdxsFormat.class.getResource("adxs.xsd"));
	}

	/**
	 * 指定したストリームをこの書式でデコードして交信記録を読み込みます。
	 * 
	 * @param in 交信記録を読み込むストリーム
	 * @return 交信記録
	 * @throws IOException 入出力時の例外
	 */
	public List<Item> decode(InputStream in) throws IOException {
		ByteArrayOutputStream buf = new ByteArrayOutputStream();
		InputStream rstream = new ReusableInputStream(in);
		StreamSource source = new StreamSource(rstream);
		try {
			schema.newValidator().validate(source);
			rstream.reset();
			former.transform(source, new StreamResult(buf));
			final byte[] raw = buf.toByteArray();
			in = new ByteArrayInputStream(raw);
			return new QxmlFormat().decode(in);
		} catch(SAXException | TransformerException ex) {
			throw new IOException(ex.getMessage(), ex);
		}
	}

	/**
	 * この操作はサポートされません。常に例外を発生します。
	 * 
	 * @param out 交信記録を書き込むストリーム
	 * @param items 出力する交信記録
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, List<Item> items) throws IOException {
		throw new UnsupportedOperationException("encoding is not supported");
	}
}
