/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.sheet;

import java.io.*;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.StringJoiner;

/**
 * 世界無線通信士財団が推奨するCabrilloサマリーシートv3の書式です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/05/03
 *
 */
public final class Cab3Format extends BaseFormat {
	/**
	 * 書式を構築します。
	 */
	public Cab3Format() {
		super("cab3");
	}

	/**
	 * 指定したストリームをこの書式でデコードして提出書類を読み込みます。
	 * 
	 * @param in 提出書類を読み込むストリーム
	 * @return 提出書類
	 * @throws IOException 入出力時の例外
	 */
	public Map<String, String> decode(InputStream in) throws IOException {
		return new Cab3FormatDecoder(in).read();
	}

	/**
	 * この書式でエンコードした提出書類を指定したストリームに書き込みます。
	 * 
	 * @param out 提出書類を書き込むストリーム
	 * @param map 出力する提出書類
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, Map<String, String> map) throws IOException {
		new Cab3FormatEncoder(out).write(new HashMap<>(map));
	}

	/**
	 * Cabrilloサマリーシートを開封するデコーダの実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2019/05/03
	 *
	 */
	private static final class Cab3FormatDecoder {
		private final LineNumberReader reader;

		/**
		 * 指定された{@link InputStream}を読み込むデコーダを構築します。
		 * 
		 * @param in 提出書類を読み込むストリーム
		 * @throws IOException UTF8に対応していない場合
		 */
		public Cab3FormatDecoder(InputStream in) throws IOException {
			this.reader = new LineNumberReader(new InputStreamReader(in, "UTF8"));
		}

		/**
		 * 読み込んだサマリーシートを開封します。
		 *
		 * @return 提出書類
		 * @throws IOException 読み込みに失敗した場合
		 */
		public Map<String, String> read() throws IOException	{
			try {
				return Collections.unmodifiableMap(head());
			} catch (IOException ex) {
				throw ex;
			} catch (Exception ex) {
				throw new IOException(ex);
			} finally {
				reader.close();
			}
		}

		/**
		 * サマリーシートの開始部分を検出します。
		 *
		 * @return 提出書類
		 * @throws IOException 読み込みに失敗した場合
		 */
		private Map<String, String> head() throws Exception {
			String line;
			while((line = reader.readLine()) != null) {
				if(!line.trim().isEmpty()) break;
			}
			if(line.trim().equals("START-OF-LOG: 2.0")) return body();
			if(line.trim().equals("START-OF-LOG: 3.0")) return body();
			throw new IOException("Tag 'START-OF-LOG:' is not found");
		}

		/**
		 * サマリーシートの内容部分を検出します。
		 *
		 * @return 提出書類
		 * @throws IOException 読み込みに失敗した場合
		 */
		private Map<String, String> body() throws Exception {
			Map<String, String> binds = new HashMap<>();
			StringJoiner qso = new StringJoiner("\n");
			String line;
			while((line = reader.readLine()) != null) {
				if(line.trim().isEmpty()) continue;
				if(line.trim().startsWith("X-")) continue;
				if(line.matches("^END-OF-LOG: *$")) break;
				String key = line.split(": ", 2)[0];
				String val = line.split(": ", 2)[1];
				if(key.equals("QSO")) qso.add(val);
				else binds.put(key, val);
			}
			if(qso.length() > 0) binds.put("QSO", qso.toString());
			return binds;
		}
	}

	/**
	 * Cabrilloサマリーシートを作成するエンコーダの実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2019/05/03
	 *
	 */
	private static final class Cab3FormatEncoder {
		private final PrintStream out;

		/**
		 * 指定された{@link OutputStream}に書き込むエンコーダを構築します。
		 * 
		 * @param out 提出書類を書き込むストリーム
		 * @throws IOException UTF8に対応していない場合
		 */
		public Cab3FormatEncoder(OutputStream out) throws IOException {
			this.out = new PrintStream(out, true, "UTF8");
		}

		/**
		 * サマリーシートを出力します。{@link Writer}は閉じられます。
		 *
		 * @param map 属性名と属性値の集合
		 * @throws IOException 入出力の例外
		 */
		public void write(Map<String, String> map) throws IOException {
			final String[] qsos = map.getOrDefault("QSO", "").split("\\R");
			out.println("START-OF-LOG: 3.0");
			for(String key: map.keySet()) {
				String val = map.get(key);
				if(key.equals("QSO")) continue;
				out.printf("%s: %s%n", key, val);
			}
			for(String qso: qsos) out.printf("QSO: %s%n", qso);
			out.println("END-OF-LOG:");
			out.flush();
			out.close();
		}
	}
}
