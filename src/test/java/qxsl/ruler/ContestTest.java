/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import qxsl.model.Item;
import qxsl.table.TableFormats;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static qxsl.ruler.Contest.ALLJA1;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Contest}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 *
 */
public final class ContestTest extends test.RandTest {
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
		final var list = new ArrayList<Arguments>();
		try(var is = RuleKit.class.getResourceAsStream(CASES)) {
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
		final var test = new RuleKit().contest(ALLJA1);
		final var sect = test.getSection(score.label);
		final var path = "allja1.".concat(fmt);
		try(var strm = RuleKit.class.getResourceAsStream(path)) {
			final var list = new TableFormats().decode(strm);
			final var sums = sect.summarize(list);
			assertThat(sums.score()).isEqualTo(score.score);
			assertThat(sums.total()).isEqualTo(score.total);
		}
	}
}
