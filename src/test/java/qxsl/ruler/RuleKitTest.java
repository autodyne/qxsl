/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.io.*;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import javax.script.ScriptException;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link RuleKit}クラスをALLJA1コンテストの規約でテストするクラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/26
 *
 */
public final class RuleKitTest extends test.RandTest {
	private final TableFormats formats = new TableFormats();
	private final Contest test;

	public RuleKitTest() throws ScriptException {
		test = new RuleKit().defined("allja1.lisp");
	}

	/**
	 * ALLJA1コンテストの部門の名前と正しい得点を格納します。
	 *
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2020/02/23
	 *
	 */
	private static final class Score {
		public final String label;
		public final int score;
		public final int total;
		public final List<String> forms;
		public Score(String line) {
			final var vals = line.split(", *", 4);
			this.score = Integer.parseInt(vals[1]);
			this.total = Integer.parseInt(vals[2]);
			this.label = vals[0];
			this.forms = Arrays.asList(vals[3].split(", *"));
		}
		@Override
		public String toString() {
			return String.format("{SECTION=%S}", this.label);
		}
	}

	/**
	 * ALLJA1コンテストの各部門の名前と得点の正解を返します。
	 *
	 *
	 * @return 部門名と得点と乗数の配列
	 * @throws IOException 通常は発生しない
	 */
	private static Score[] testMethodSource() throws IOException {
		final URL path = RuleKit.class.getResource("allja1.test");
		final Reader r = new InputStreamReader(path.openStream());
		try (BufferedReader br = new BufferedReader(r)) {
			return br.lines().map(Score::new).toArray(Score[]::new);
		}
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void test(Score score) throws Exception {
		final Section sect = test.getSection(score.label);
		for (String format: score.forms) {
			final String path = "allja1.".concat(format);
			final URL url = getClass().getResource(path);
			try (InputStream strm = url.openStream()) {
				var sum = sect.summarize(formats.decode(strm));
				assertThat(sum.score()).isEqualTo(score.score);
				assertThat(test.score(sum)).isEqualTo(score.total);
			}
		}
	}
}
