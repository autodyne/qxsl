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

import qxsl.model.Item;

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
	@MethodSource({"constraintsJA1", "constraintsRTC"})
	public void testSummarize(Constraint cs, String fmt) {
		final var rule = cs.getContest().section(cs.rule);
		final var sums = rule.summarize(cs.getItems(fmt));
		assertThat(sums.score()).isEqualTo(cs.score);
		assertThat(sums.total()).isEqualTo(cs.total);
	}

	@Test
	public void testGet() {
		final var rule = RuleKit.load("allja1.lisp").contest();
		assertThat(rule.get("DIGIT")).isInstanceOf(String.class);
		assertThat(rule.get("split")).isInstanceOf(Method.class);
	}

	private static final List<Arguments> constraintsJA1() {
		return load("allja1");
	}

	private static final List<Arguments> constraintsRTC() {
		return load("online");
	}

	private static final List<Arguments> load(String name) {
		final var list = new ArrayList<Arguments>();
		final var util = new AssetUtils(Contest.class);
		for(final var v: util.listLines(name.concat(".test"))) {
			final var cs = new Constraint(name, v.split(", +", 4));
			for(var fmt: cs.forms) list.add(Arguments.of(cs, fmt));
		}
		return list;
	}

	private static final class Constraint {
		public final String name;
		public final String rule;
		public final int score;
		public final int total;
		public final String[] forms;

		public Constraint(String name, String[] vals) {
			this.name = name;
			this.rule = vals[0];
			this.score = Integer.parseInt(vals[1]);
			this.total = Integer.parseInt(vals[2]);
			this.forms = vals[3].split(":");
		}

		public Contest getContest() {
			return RuleKit.load(name.concat(".lisp")).contest();
		}

		public List<Item> getItems(String format) {
			final var path = name.concat(".").concat(format);
			return new AssetUtils(Contest.class).items(path);
		}

		@Override
		public final String toString() {
			return String.format("%s %s", name, rule);
		}
	}
}
