/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;
import javax.script.ScriptException;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static java.nio.charset.StandardCharsets.UTF_8;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link RuleKit}クラスをALLJA1コンテストの規約でテストするクラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 *
 */
public final class RuleKitTest extends test.RandTest {
	private final TableFormats formats = new TableFormats();
	private final Contest test;

	public RuleKitTest() throws ScriptException {
		test = new RuleKit().contest("allja1.lisp");
	}

	/**
	 * ALLJA1コンテストの部門の名前と正しい得点を格納します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/23
	 *
	 */
	private static final class Result {
		public final String label;
		public final int score;
		public final int total;
		public final List<String> forms;
		public Result(String line) {
			final var vals = line.split(", *", 4);
			this.score = Integer.parseInt(vals[1]);
			this.total = Integer.parseInt(vals[2]);
			this.label = vals[0];
			this.forms = Arrays.asList(vals[3].split(", *"));
		}
		@Override
		public String toString() {
			return String.format("{SECTION=%s}", this.label);
		}
		private Stream<Pair> pairs() {
			return forms.stream().map(f -> new Pair(this, f));
		}
	}

	/**
	 * ALLJA1コンテストの部門の正解と書式のペアを格納します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/02/27
	 *
	 */
	private static final class Pair {
		public final Result result;
		public final String format;
		public Pair(Result result, String format) {
			this.result = result;
			this.format = format;
		}
		@Override
		public String toString() {
			return String.format("{%s (FORMAT=%s)}", result, format);
		}
	}

	/**
	 * ALLJA1コンテストの各部門の名前と得点の正解を返します。
	 *
	 *
	 * @return 部門名と得点と乗数の配列
	 * @throws IOException 通常は発生しない
	 */
	private static Pair[] testMethodSource() throws IOException {
		try (var is = RuleKit.class.getResourceAsStream("allja1.test")) {
			final var ir = new InputStreamReader(is, UTF_8);
			final var br = new BufferedReader(ir);
			final Stream<Result> results = br.lines().map(Result::new);
			return results.flatMap(Result::pairs).toArray(Pair[]::new);
		}
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void test(Pair pair) throws IOException {
		final Section sect = test.getSection(pair.result.label);
		final String path = "allja1.".concat(pair.format);
		try (var strm = getClass().getResourceAsStream(path)) {
			final var sum = sect.summarize(formats.decode(strm));
			assertThat(sum.score()).isEqualTo(pair.result.score);
			assertThat(test.score(sum)).isEqualTo(pair.result.total);
		}
	}
}
