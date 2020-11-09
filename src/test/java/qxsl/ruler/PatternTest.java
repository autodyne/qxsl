/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import qxsl.draft.Mode;
import qxsl.draft.Name;
import qxsl.draft.Time;
import qxsl.model.Item;
import qxsl.table.TableFactory;
import qxsl.table.TableManager;

import gaas.utils.AssetUtils;

/**
 * {@link Pattern}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/14
 */
public final class PatternTest extends Assertions {
	private static final Pattern rule = load();

	/**
	 * テスト対象である交信記録の変換処理の実装を取得します。
	 *
	 *
	 * @return 変換処理の実装
	 */
	private static final Pattern load() {
		return RuleKit.load("jautil.lisp").pattern();
	}

	/**
	 * 書式変換を試みる交信記録とその書式のリストを返します。
	 *
	 *
	 * @return 交信記録と書式のリスト
	 */
	private static final List<Arguments> items() {
		final var fmts = new TableManager();
		final var list = new ArrayList<Arguments>();
		final var util = new AssetUtils(Pattern.class);
		for(final var item: util.items("allja1.qxml")) {
			for(var f: fmts) list.add(Arguments.of(item, f));
		}
		return list;
	}

	@ParameterizedTest
	@MethodSource("items")
	public void test(Item item, TableFactory fmt) {
		final var seq1 = fmt.encode(rule.transform(item, fmt.type()));
		final var seq2 = rule.normalize(fmt.decode(seq1), fmt.type());
		final var back = seq2.get(0);
		back.set(Name.from(item));
		if(Mode.from(back).isRTTY()) back.set(Mode.from(item));
		item.set(Time.from(item).ofYear(2020).copyDropSecond());
		back.set(Time.from(back).ofYear(2020).copyDropSecond());
		assertThat(item).isEqualTo(back);
	}

	@Test
	public void testGet() {
		final var rule = RuleKit.load("jautil.lisp").pattern();
		assertThat(rule.get("PHONE")).isInstanceOf(String.class);
		assertThat(rule.get("match")).isInstanceOf(Method.class);
	}
}
