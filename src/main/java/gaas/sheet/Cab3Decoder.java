/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.sheet;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;
import java.util.StringJoiner;

import qxsl.sheet.SheetDecoder;

/**
 * Cabrilloサマリーシートを開封するデコーダの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/03
 */
public final class Cab3Decoder implements SheetDecoder {
	private final Map<String, StringJoiner> values;
	private final BufferedReader source;
	private final Cab3Factory format;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 入力
	 * @param format 書式
	 */
	public Cab3Decoder(Reader reader, Cab3Factory format) {
		this.source = new BufferedReader(reader);
		this.values = new HashMap<>();
		this.format = format;
	}

	/**
	 * ストリームを閉じて資源を解放します。
	 *
	 *
	 * @throws IOException 解放に失敗した場合
	 */
	@Override
	public final void close() throws IOException {
		source.close();
	}

	/**
	 * 指定された属性の値を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 属性の値
	 */
	@Override
	public final byte[] getBinary(String key) {
		return format.stringToByteArray(getString(key));
	}

	/**
	 * 指定された属性の値を返します。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @return 属性の値
	 */
	@Override
	public final String getString(String key) {
		return getBuffer(key).toString();
	}

	/**
	 * ストリームの要約書類を読み取ります。
	 *
	 *
	 * @return 要約書類を読み取ったデコーダ
	 *
	 * @throws IOException 構文上または読取り時の例外
	 */
	@Override
	public final SheetDecoder decode() throws IOException {
		final var line = source.readLine().trim();
		if(line.equals("START-OF-LOG: 2.0")) return body();
		if(line.equals("START-OF-LOG: 3.0")) return body();
		throw new IOException("'START-OF-LOG:' is not found");
	}

	/**
	 * 指定された属性に対応するバッファを返します。
	 *
	 *
	 * @param name 属性の名前
	 *
	 * @return バッファ
	 */
	private final StringJoiner getBuffer(String name) {
		if(!values.containsKey(name)) {
			final var line = System.lineSeparator();
			final var join = new StringJoiner(line);
			return values.compute(name, (k, v) -> join);
		} else return values.get(name);
	}

	/**
	 * 要約書類の属性とその値の並びを読み取ります。
	 *
	 *
	 * @return このデコーダ
	 *
	 * @throws IOException 読み込みに失敗した場合
	 */
	private final SheetDecoder body() throws IOException {
		String line;
		while((line = source.readLine()) != null) {
			if(line.matches("^END-OF-LOG: *$")) break;
			final var key = line.split(": ", 2)[0];
			final var val = line.split(": ", 2)[1];
			final var buf = getBuffer(key);
			buf.add(key.equals("QSO")? line: val);
		}
		return this;
	}
}
