/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.StringJoiner;
import java.util.concurrent.Callable;

import qxsl.ruler.RuleKit;
import qxsl.sheet.SheetOrTable;
import qxsl.table.TableManager;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Parameters;

/**
 * 指定された交信記録の内容を指定された書式に変換します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/09
 */
@Command(name = "qxsl", description = {"contest log processor"})
public final class CommandApp implements Callable<Integer> {
	@Parameters
	private Path source;
	@Parameters
	private Path target;
	@Parameters(description = {"${CANDIDATES}"})
	private String format;

	/**
	 * 取扱説明を準備します。
	 *
	 *
	 * @since 2022/06/27
	 */
	public CommandApp() {
		final var join = new StringJoiner(", ");
		for(var f: new TableManager()) join.add(f.type());
		System.setProperty("CANDIDATES", join.toString());
	}

	/**
	 * 交信記録を処理します。
	 *
	 *
	 * @return 終了コード
	 *
	 * @throws IOException 入出力に失敗した場合
	 */
	@Override
	public Integer call() throws IOException {
		final var util = RuleKit.load("jautil.lisp").pattern();
		final var pack = Files.readAllBytes(source);
		final var list = new SheetOrTable().unpack(pack);
		final var norm = util.normalize(list, null);
		final var form = new TableManager().factory(format);
		final var data = util.transform(norm, form.type());
		Files.write(target, form.encode(data));
		return 0;
	}

	/**
	 * コマンドラインツールを起動します。
	 *
	 *
	 * @param args コマンドライン引数
	 */
	public static final void main(String[] args) {
		System.exit(new CommandLine(new CommandApp()).execute(args));
	}
}
