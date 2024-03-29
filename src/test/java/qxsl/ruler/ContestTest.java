/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.ruler;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.TimeZone;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import qxsl.model.Item;
import qxsl.utils.AssetUtil;

/**
 * {@link Contest}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/14
 */
public final class ContestTest extends Assertions {
	private static final String JST = "Asia/Tokyo";
	private final TimeZone timeZone;
	private final TimeZone testZone;

	public ContestTest() {
		this.timeZone = TimeZone.getDefault();
		this.testZone = TimeZone.getTimeZone(JST);
	}

	@BeforeEach
	public void prepareSystemTimeZone() {
		TimeZone.setDefault(timeZone);
	}

	@AfterEach
	public void restoreSystemTimeZone() {
		TimeZone.setDefault(testZone);
	}

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
		final var util = new AssetUtil(Contest.class);
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
			return new AssetUtil(Contest.class).items(path);
		}

		@Override
		public final String toString() {
			return String.format("%s %s", name, rule);
		}
	}
}
