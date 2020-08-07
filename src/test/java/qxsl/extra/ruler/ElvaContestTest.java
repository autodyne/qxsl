/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.ruler;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import qxsl.table.TableFormats;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.List;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * {@link ElvaContest}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class ElvaContestTest extends org.assertj.core.api.Assertions {
	private static final Class<?> CLS = ElvaRuleKit.class;
	private static final String CASES = "allja1.test";

	private static final class Score {
		public final String label;
		public final int score;
		public final int total;
		public final String[] forms;
		public Score(String...vals) {
			this.label = vals[0];
			this.score = Integer.parseInt(vals[1]);
			this.total = Integer.parseInt(vals[2]);
			this.forms = vals[3].split(":");
		}
		@Override
		public final String toString() {
			return this.label;
		}
	}

	/**
	 * 得点を計算する部門と正解をクラスパスから読み出します。
	 *
	 *
	 * @return 部門と正解のリスト
	 */
	private static List<Arguments> scores() throws Exception {
		final var list = new LinkedList<Arguments>();
		try(var is = CLS.getResourceAsStream(CASES)) {
			final var ir = new InputStreamReader(is, UTF_8);
			final var br = new BufferedReader(ir);
			for(var value: br.lines().toArray(String[]::new)) {
				final Score s = new Score(value.split(", +", 4));
				for(var f: s.forms) list.add(Arguments.of(s, f));
			}
		}
		return list;
	}

	@ParameterizedTest
	@MethodSource("scores")
	public void test(Score score, String fmt) throws Exception {
		final var strm = CLS.getResourceAsStream("allja1.lisp");
		final var read = new InputStreamReader(strm);
		final var test = new ElvaRuleKit().contest(read);
		final var sect = test.getSection(score.label);
		final var path = "allja1.".concat(fmt);
		try(var res = CLS.getResourceAsStream(path)) {
			final var list = new TableFormats().decode(res);
			final var sums = sect.summarize(list);
			assertThat(sums.score()).isEqualTo(score.score);
			assertThat(sums.total()).isEqualTo(score.total);
		}
	}
}
