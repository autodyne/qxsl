/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.sheet;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;
import java.util.StringJoiner;

import qxsl.sheet.PrintDecoder;
import qxsl.sheet.SheetDecoder;

/**
 * Cabrillo書式で永続化された要約書類を解読します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/03
 */
public final class Cab3Decoder extends PrintDecoder {
	private final Map<String, StringJoiner> values;
	private final BufferedReader source;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 入力
	 */
	public Cab3Decoder(Reader reader) {
		super("cab3", "ASCII");
		this.values = new HashMap<>();
		this.source = new BufferedReader(reader);
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
