/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.*;
import java.net.URL;
import java.util.List;
import javax.xml.transform.*;
import javax.xml.transform.stream.*;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.xml.sax.SAXException;
import qxsl.model.Item;
import qxsl.table.TableFormat;
import qxsl.table.TableFormats;

/**
 * ADIFのうちADXと呼ばれる新方式の書式の部分的な実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/27
 *
 */
public final class AdxsFormat extends BaseFormat {
	private final String XSDPATH = "adxs.xsd";
	private final String XSLPATH = "adxs.xsl";
	private final Schema schema;
	private final TableFormats tables;

	/**
	 * ライブラリからスキーマ定義を読み込んで書式を構築します。
	 *
	 * @throws SAXException スキーマ定義の例外
	 */
	public AdxsFormat() throws SAXException {
		super("adxs");
		SchemaFactory sf = SchemaFactory.newDefaultInstance();
		final URL url = AdxsFormat.class.getResource(XSDPATH);
		this.schema = sf.newSchema(url);
		this.tables = new TableFormats();
	}

	@Override
	public TableDecoder decoder(InputStream is) {
		try {
			return new AdxsDecoder(is);
		} catch (TransformerException ex) {
			throw new UnsupportedOperationException(ex);
		}
	}

	@Override
	public TableEncoder encoder(OutputStream os) {
		try {
			return new AdxsEncoder(os);
		} catch (TransformerException ex) {
			throw new UnsupportedOperationException(ex);
		}
	}

	/**
	 * ADXサブセット書式で直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 *
	 */
	private final class AdxsDecoder implements TableDecoder {
		private final InputStream stream;
		private final Transformer format;

		/**
		 * 指定されたストリームから交信記録を読み込むデコーダを構築します。
		 *
		 * @param is 交信記録を読み込むストリーム
		 * @throws TransformerException 通常は発生しない例外
		 */
		public AdxsDecoder(InputStream is) throws TransformerException {
			this.stream = is;
			TransformerFactory tf = TransformerFactory.newInstance();
			String xslt = getClass().getResource(XSLPATH).toString();
			this.format = tf.newTransformer(new StreamSource(xslt));
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException リソースの解放に失敗した場合
		 */
		@Override
		public final void close() throws IOException {
			stream.close();
		}

		/**
		 * ストリームの内容を検証してから交信記録を読み込みます。
		 *
		 * @return 読み込んだ交信記録
		 * @throws IOException 構文の問題もしくは読み込みに失敗した場合
		 */
		@Override
		public final List<Item> decode() throws IOException {
			try {
				return valid(stream.readAllBytes());
			} catch (IOException ex) {
				throw ex;
			} catch (Exception ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * ストリームの内容を検証してから交信記録を読み込みます。
		 *
		 * @param data ストリームの内容
		 * @return 読み込んだ交信記録
		 * @throws Exception 構文の問題もしくは読み込みに失敗した場合
		 */
		private final List<Item> valid(byte[] data) throws Exception {
			final var stream = new ByteArrayInputStream(data);
			final var buffer = new ByteArrayOutputStream();
			final var source = new StreamSource(stream);
			final var result = new StreamResult(buffer);
			schema.newValidator().validate(source);
			stream.reset();
			this.format.transform(source, result);
			return tables.decode(buffer.toByteArray());
		}
	}

	/**
	 * ADXサブセット書式で直列化された交信記録をエンコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 *
	 */
	private final class AdxsEncoder implements TableEncoder {
		private final StreamResult result;
		private final Transformer format;

		/**
		 * 指定されたストリームに交信記録を書き出すデコーダを構築します。
		 *
		 * @param os 交信記録を出力するストリーム
		 * @throws TransformerException 通常は発生しない例外
		 */
		public AdxsEncoder(OutputStream os) throws TransformerException {
			TransformerFactory tf = TransformerFactory.newInstance();
			String xslt = getClass().getResource(XSLPATH).toString();
			this.format = tf.newTransformer(new StreamSource(xslt));
			this.result = new StreamResult(os);
		}

		/**
		 * ストリームを閉じてリソースを解放します。
		 *
		 * @throws IOException リソース解放に失敗した場合
		 */
		@Override
		public final void close() throws IOException {
			result.getOutputStream().close();
		}

		/**
		 * 交信記録を出力します。
		 *
		 * @param items 交信記録
		 * @throws IOException 出力に失敗した場合
		 */
		@Override
		public void encode(List<Item> items) throws IOException {
			try {
				this.trans(items);
			} catch (IOException ex) {
				throw ex;
			} catch (Exception ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * 交信記録を出力します。
		 *
		 * @param items 交信記録
		 * @throws Exception 出力に失敗した場合
		 */
		private void trans(List<Item> items) throws Exception {
			final var buff = new ByteArrayOutputStream();
			tables.forName("qxml").encoder(buff).encode(items);
			final var data = buff.toByteArray();
			final var strm = new ByteArrayInputStream(data);
			format.transform(new StreamSource(strm), result);
		}
	}
}
