/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.extra.sheet;

import java.io.*;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.StringJoiner;

/**
 * Cabrilloサマリーシートv3(v2)の書式です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/03
 *
 */
public final class Cab3Format extends BaseFormat {
	public Cab3Format() {
		super("cab3");
	}

	@Override
	public SheetDecoder decoder(Reader reader) {
		return new Cab3Decoder(reader);
	}

	@Override
	public SheetEncoder encoder(Writer writer) {
		return new Cab3Encoder(writer);
	}

	/**
	 * Cabrilloサマリーシートを開封するデコーダの実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/03
	 *
	 */
	private final class Cab3Decoder implements SheetDecoder {
		private final BufferedReader reader;

		/**
		 * 指定されたリーダを読み込むデコーダを構築します。
		 *
		 * @param reader 要約書類を読み込むリーダ
		 */
		public Cab3Decoder(Reader reader) {
			this.reader = new BufferedReader(reader);
		}

		/**
		 * リーダを閉じてリソースを解放します。
		 *
		 * @throws IOException リソースの解放に失敗した場合
		 */
		@Override
		public void close() throws IOException {
			reader.close();
		}

		/**
		 * 読み込んだサマリーシートを開封します。
		 *
		 * @return 要約書類
		 * @throws IOException 読み込みに失敗した場合
		 */
		@Override
		public Map<String, String> decode() throws IOException {
			try {
				return Collections.unmodifiableMap(head());
			} catch (IOException ex) {
				throw ex;
			} catch (Exception ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * サマリーシートの開始部分を検出します。
		 *
		 * @return 要約書類
		 * @throws IOException 読み込みに失敗した場合
		 */
		private Map<String, String> head() throws Exception {
			String line;
			while((line = reader.readLine()) != null && line.isBlank());
			if(line.trim().equals("START-OF-LOG: 2.0")) return body();
			if(line.trim().equals("START-OF-LOG: 3.0")) return body();
			throw new IOException("Tag 'START-OF-LOG:' is not found");
		}

		/**
		 * サマリーシートの内容部分を検出します。
		 *
		 * @return 要約書類
		 * @throws IOException 読み込みに失敗した場合
		 */
		private Map<String, String> body() throws Exception {
			Map<String, String> binds = new HashMap<>();
			StringJoiner qso = new StringJoiner("\n");
			String line;
			while((line = reader.readLine()) != null) {
				if(!line.isBlank() && !line.startsWith("X-")) {
					if(line.matches("^END-OF-LOG: *$")) break;
					String key = line.split(": ", 2)[0];
					String val = line.split(": ", 2)[1];
					if(key.equals("QSO")) qso.add(line);
					else binds.put(key, val);
				}
			}
			binds.put("QSO", qso.toString());
			return binds;
		}
	}

	/**
	 * Cabrilloサマリーシートを作成するエンコーダの実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/05/03
	 *
	 */
	private final class Cab3Encoder implements SheetEncoder {
		private final PrintWriter writer;

		/**
		 * 指定されたライタに書き込むエンコーダを構築します。
		 *
		 * @param writer 要約書類を出力するライタ
		 */
		public Cab3Encoder(Writer writer) {
			this.writer = new PrintWriter(writer, true);
		}

		/**
		 * ライタを閉じてリソースを解放します。
		 *
		 * @throws IOException リソースの解放に失敗した場合
		 */
		@Override
		public void close() throws IOException {
			writer.close();
		}

		/**
		 * サマリーシートを出力します。
		 *
		 * @param map 属性名と属性値の集合
		 * @throws IOException 入出力の例外
		 */
		@Override
		public void encode(Map<String, String> map) throws IOException {
			final String qsos = map.getOrDefault("QSO", "");
			writer.println("START-OF-LOG: 3.0");
			for(String key: map.keySet()) {
				String val = map.get(key);
				if(key.equals("QSO")) continue;
				writer.printf("%s: %s%n", key, val);
			}
			writer.println(qsos);
			writer.println("END-OF-LOG:");
			writer.flush();
		}
	}
}
