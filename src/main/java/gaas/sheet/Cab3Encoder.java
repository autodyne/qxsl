/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.sheet;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

import qxsl.sheet.PrintEncoder;

/**
 * 所定の構造の要約書類をCabrillo書式で永続化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/03
 */
public final class Cab3Encoder extends PrintEncoder {
	private final Map<String, String> values;
	private final PrintWriter target;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param writer 出力
	 */
	public Cab3Encoder(Writer writer) {
		super("cab3", "ASCII");
		this.values = new HashMap<>();
		this.target = new PrintWriter(writer, true);
	}

	/**
	 * ストリームを閉じて資源を解放します。
	 *
	 *
	 * @throws IOException 解放に失敗した場合
	 */
	@Override
	public final void close() throws IOException {
		target.close();
	}

	/**
	 * 指定された属性と属性値を設定します。
	 *
	 *
	 * @param key 属性の名前
	 * @param val 属性の値
	 */
	@Override
	public final void set(String key, String val) {
		values.put(key, val);
	}

	/**
	 * ストリームに要約書類を書き込みます。
	 *
	 *
	 * @throws IOException 書き込み時の例外
	 */
	@Override
	public final void encode() throws IOException {
		target.println("START-OF-LOG: 3.0");
		for(var key: values.keySet()) {
			final var val = values.get(key);
			if(key.equals("QSO")) target.print(val);
			else target.printf("%s: %s%n", key, val);
		}
		target.println("END-OF-LOG:");
		target.flush();
	}
}
