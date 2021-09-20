/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.alone;

import java.util.concurrent.Callable;

import gaas.morse.MorseTone;

import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;

/**
 * 指定された文字列をモールス符号に変換して音響再生します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2021/09/15
 */
@Command(name = "melody", description = {"generate Morse tone"})
public final class Melody implements Callable<Integer> {
	@Option(names = {"--rate"})
	private int rate = 8000;
	@Option(names = {"--tone"})
	private int tone = 800;
	@Option(names = {"--wpms"})
	private int wpms = 25;
	@Parameters(index = "0", description = {"text"})
	private String text;

	/**
	 * サブコマンドを実行して終了コードを返します。
	 *
	 *
	 * @return 終了コード
	 */
	@Override
	public Integer call() throws Exception {
		new MorseTone(rate, tone, wpms).generate(text).play();
		return 0;
	}
}
