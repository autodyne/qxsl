/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.io.Reader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.namespace.QName;

import qxsl.model.Item;
import qxsl.table.PrintDecoder;
import qxsl.value.Field;

import static gaas.table.AdisFactory.EOH;
import static gaas.table.AdisFactory.EOR;
import static gaas.table.AdisFactory.URI;

/**
 * ADI書式で永続化された交信記録を解読します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/08
 */
public final class AdisDecoder extends PrintDecoder {
	private final Pattern pattern;
	private boolean isValid;

	/**
	 * 指定された入力を読み込むデコーダを構築します。
	 *
	 *
	 * @param reader 入力
	 */
	public AdisDecoder(Reader reader) {
		super("adis", reader);
		this.pattern = Pattern.compile(get("regex"));
	}

	/**
	 * ストリームの交信記録の冒頭を読み取ります。
	 *
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void head() throws IOException {
		final boolean head = this.peek() != '<';
		if(head) isValid = collect(EOH) != null;
	}

	/**
	 * ストリームの交信記録の末尾を読み取ります。
	 *
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final void foot() throws IOException {
		final var error = "no header/records detected";
		if(!this.isValid) throw new IOException(error);
	}

	/**
	 * ストリームの現在位置の交信記録を読み取ります。
	 *
	 *
	 * @return 読み取った交信記録
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final Item next() throws IOException {
		final var item = new Item();
		final var text = collect(EOR);
		final var iter = pattern.matcher(text).reset();
		while(iter.find()) item.set(field(iter, text));
		return item;
	}

	/**
	 * ストリームに交信記録が存在するかを確認します。
	 *
	 *
	 * @return 交信記録を読み取れる場合は真
	 *
	 * @throws IOException 構文上または読取り時の例外
	 *
	 * @since 2020/09/04
	 */
	@Override
	public final boolean hasNext() throws IOException {
		final var exists = collect(EOR) != null;
		this.isValid |= exists;
		super.reset();
		return exists;
	}

	/**
	 * 構文解析器が参照する現在位置から属性を読み取ります。
	 *
	 *
	 * @param match 正規表現の解析器
	 * @param field 属性が並ぶ文字列
	 *
	 * @return 読み取った属性
	 *
	 * @since 2020/09/06
	 */
	private final Field field(Matcher match, String field) {
		final var sub = field.substring(match.end());
		final var key = match.group(1).toUpperCase();
		final var num = Integer.parseInt(match.group(2));
		return cache(new QName(URI, key), sub.substring(0, num));
	}
}
