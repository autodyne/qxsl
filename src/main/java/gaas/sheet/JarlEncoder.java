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

import static gaas.sheet.JarlFactory.SUM;

/**
 * 所定の構造の要約書類をJARL書式で永続化します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/11
 */
public final class JarlEncoder extends PrintEncoder {
	private final Map<String, String> values;
	private final PrintWriter target;
	private final String LOGKEY;

	/**
	 * 指定された出力に書き込むエンコーダを構築します。
	 *
	 *
	 * @param writer 出力
	 */
	public JarlEncoder(Writer writer) {
		super("jarl", "SJIS");
		this.LOGKEY = getTableKey();
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
		target.printf("<%s VERSION=R2.0>%n", SUM);
		for(var key: values.keySet()) writeTag(key);
		target.printf("</%s>%n", SUM);
		writeLogSheetItems();
		target.flush();
	}

	/**
	 * 指定された名前の属性をストリームに書き込みます。
	 *
	 *
	 * @param key 属性の名前
	 *
	 * @throws IOException 書き込み時の例外
	 */
	private final void writeTag(String key) throws IOException {
		if(!LOGKEY.equalsIgnoreCase(key)) {
			final var temp = "<%1$s>%2$s</%1$s>%n";
			target.printf(temp, key, values.get(key));
		}
	}

	/**
	 * 交信記録を表す文字列をストリームに書き込みます。
	 *
	 *
	 * @throws IOException 書き込み時の例外
	 */
	private final void writeLogSheetItems() throws IOException {
		final var table = values.getOrDefault(LOGKEY, "");
		target.printf("<%1$s>%n%2$s%n</%1$s>", LOGKEY, table);
	}
}
