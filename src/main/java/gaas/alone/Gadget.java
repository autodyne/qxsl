/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.alone;

import java.util.concurrent.Callable;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Spec;

/**
 * コマンドラインツールが提供するコマンド群を定義します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/09
 */
@Command(name = "qxsl", subcommands = {Format.class, Melody.class})
public final class Gadget implements Callable<Integer> {
	@Spec
	CommandSpec spec;

	/**
	 * 使用方法の説明を表示します。
	 *
	 *
	 * @return 終了コード
	 */
	@Override
	public Integer call() {
		spec.commandLine().usage(System.err);
		return 1;
	}

	/**
	 * コマンドラインツールを起動します。
	 *
	 *
	 * @param args コマンドライン引数
	 */
	public static final void main(String[] args) {
		System.exit(new CommandLine(new Gadget()).execute(args));
	}
}
