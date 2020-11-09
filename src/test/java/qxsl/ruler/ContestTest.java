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

import gaas.utils.AssetUtils;

/**
 * {@link Contest}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/14
 */
public final class ContestTest extends Assertions {
	/**
	 * コンテストの部門の名前と正しい得点を格納します。
	 *
	 *
	 * @since 2020/02/23
	 */
	private static final class Constraint {
		public final String label;
		public final int score;
		public final int total;
		public final String[] forms;

		/**
		 * 部門の名前と素点と総得点を指定します。
		 *
		 * @param vals 名前と素点と総得点の配列
		 */
		public Constraint(String...vals) {
			this.label = vals[0];
			this.score = Integer.parseInt(vals[1]);
			this.total = Integer.parseInt(vals[2]);
			this.forms = vals[3].split(":");
		}

		/**
		 * テスト項目として文字列表現を返します。
		 *
		 *
		 * @return 文字列表現
		 */
		@Override
		public final String toString() {
			return label;
		}
	}

	/**
	 * 得点を計算する部門と正解をクラスパスから読み出します。
	 *
	 *
	 * @return 部門と正解のリスト
	 */
	private static final List<Arguments> constraints() {
		final var list = new ArrayList<Arguments>();
		final var util = new AssetUtils(Contest.class);
		util.lines("allja1.test").forEach(ln -> {
			final var v = new Constraint(ln.split(", +", 4));
			for(var f: v.forms) list.add(Arguments.of(v, f));
		});
		return list;
	}

	@ParameterizedTest
	@MethodSource("constraints")
	public void test(Constraint cs, String fmt) {
		final var path = "allja1.".concat(fmt);
		final var rule = RuleKit.load("allja1.lisp");
		final var util = new AssetUtils(Contest.class);
		final var sect = rule.contest().section(cs.label);
		final var sums = sect.summarize(util.items(path));
		assertThat(sums.score()).isEqualTo(cs.score);
		assertThat(sums.total()).isEqualTo(cs.total);
	}

	@Test
	public void testGet() {
		final var rule = RuleKit.load("allja1.lisp").contest();
		assertThat(rule.get("DIGIT")).isInstanceOf(String.class);
		assertThat(rule.get("split")).isInstanceOf(Method.class);
	}
}
