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

	private static final List<Arguments> constraints() {
		final var list = new ArrayList<Arguments>();
		final var util = new AssetUtils(Contest.class);
		for (final var ln : util.listLines("allja1.test")) {
			final var v = new Constraint(ln.split(", +", 4));
			for (var f : v.forms) list.add(Arguments.of(v, f));
		}
		return list;
	}

	private static final class Constraint {
		public final String label;
		public final int score;
		public final int total;
		public final String[] forms;

		public Constraint(String... vals) {
			this.label = vals[0];
			this.score = Integer.parseInt(vals[1]);
			this.total = Integer.parseInt(vals[2]);
			this.forms = vals[3].split(":");
		}

		@Override
		public final String toString() {
			return label;
		}
	}
}
