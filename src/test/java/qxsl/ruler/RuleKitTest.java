/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.List;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import qxsl.model.Item;
import qxsl.table.TableManager;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * {@link RuleKit}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class RuleKitTest extends Assertions {
	private static final Class<?> CLS = RuleKit.class;
	private static final String CASES = "allja1.test";
	private static final String RULES = "allja1.rb";
	private static final String ITEMS = "allja1.qxml";
	private static final Contest RULE = loadContest();

	@Test
	public void testName() {
		assertThat(RuleKit.load("elva").name()).isEqualTo("elva");
		assertThat(RuleKit.load("ruby").name()).isEqualTo("ruby");
	}

	/**
	 * テスト対象のコンテスト規約を読み出します。
	 *
	 *
	 * @return 規約
	 */
	private static final Contest loadContest() {
		final var kit = RuleKit.load("ruby");
		final var res = CLS.getResourceAsStream(RULES);
		return kit.contest(new InputStreamReader(res));
	}

	/**
	 * コンテストの部門の名前と正しい得点を格納します。
	 *
	 *
	 * @since 2020/02/23
	 */
	private static final class Line {
		public final String label;
		public final int score;
		public final int total;
		public final String[] forms;

		/**
		 * 部門の名前と素点と総得点を指定します。
		 *
		 * @param vals 名前と素点と総得点の配列
		 */
		public Line(String...vals) {
			this.label = vals[0];
			this.score = Integer.parseInt(vals[1]);
			this.total = Integer.parseInt(vals[2]);
			this.forms = vals[3].split(":");
		}

		/**
		 * テスト項目として文字列表現を返します。
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
	private static List<Arguments> lines() throws Exception {
		final var list = new LinkedList<Arguments>();
		try(var is = CLS.getResourceAsStream(CASES)) {
			final var ir = new InputStreamReader(is, UTF_8);
			final var br = new BufferedReader(ir);
			for(var correct: br.lines().toArray(String[]::new)) {
				final Line s = new Line(correct.split(", +", 4));
				for(var f: s.forms) list.add(Arguments.of(s, f));
			}
		}
		return list;
	}

	/**
	 * 書式変換を試みる交信記録とその書式のリストを返します。
	 *
	 *
	 * @return 交信記録と書式のリスト
	 */
	private static List<Arguments> items() throws Exception {
		final var fmts = new TableManager();
		final var list = new LinkedList<Arguments>();
		try(var res = CLS.getResourceAsStream(ITEMS)) {
			for(var f: fmts) for(var i: fmts.decode(res)) {
				list.add(Arguments.of(i, f.getName()));
			}
		}
		return list;
	}

	@ParameterizedTest
	@MethodSource("lines")
	public void testALLJA1(Line line, String fmt) throws Exception {
		final var sect = RULE.getSection(line.label);
		final var path = "allja1.".concat(fmt);
		final var fmts = new TableManager();
		try(var res = CLS.getResourceAsStream(path)) {
			final var sums = sect.summarize(fmts.decode(res));
			assertThat(sums.score()).isEqualTo(line.score);
			assertThat(sums.total()).isEqualTo(line.total);
		}
	}

	@ParameterizedTest
	@MethodSource("items")
	public void testFormat(Item item, String fmt) throws Exception {
		final var sect = RULE.getSection("1エリア内 個人 総合 部門");
		final var fact = new TableManager().forName(fmt);
		final var list = fact.encode(sect.encode(item, fact));
		final var back = sect.decode(fact.decodeSingle(list));
		final var msg1 = sect.verify(item).toString();
		final var msg2 = sect.verify(back).toString();
		assertThat(msg1).isEqualTo(msg2);
	}
}
