/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.StringJoiner;
import java.util.concurrent.Callable;

import qxsl.ruler.RuleKit;
import qxsl.sheet.SheetOrTable;
import qxsl.table.TableFactory;
import qxsl.table.TableManager;

/**
 * 交信記録を指定された書式に変換するコマンドです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/09
 */
public final class ShellUtil implements Callable<Integer> {
	private static final String PATTERN = "format.lisp";
	private final TableManager tables;
	private final TableFactory format;
	private final Path source;
	private final Path target;

	/**
	 * コマンドを準備します。
	 *
	 *
	 * @param args 引数
	 *
	 * @since 2022/07/24
	 */
	public ShellUtil(String[] args) {
		try {
			this.tables = new TableManager();
			this.source = Paths.get(args[0]);
			this.target = Paths.get(args[1]);
			this.format = tables.factory(args[2]);
		} catch (IndexOutOfBoundsException ex) {
			throw help();
		}
	}

	/**
	 * コマンドを実行します。
	 *
	 *
	 * @return 終了コード
	 *
	 * @throws IOException 入出力に失敗した場合
	 */
	@Override
	public final Integer call() throws IOException {
		final var util = RuleKit.load(PATTERN).pattern();
		final var file = Files.readAllBytes(this.source);
		final var list = new SheetOrTable().unpack(file);
		final var norm = util.normalize(list, null);
		final var data = util.transform(norm, format.type());
		Files.write(this.target, format.encode(data));
		return 0;
	}

	/**
	 * 使用方法を説明する例外を発生させます。
	 *
	 *
	 * @return RuntimeException 例外
	 *
	 * @since 2022/07/24
	 */
	public final RuntimeException help() {
		final var join = new StringJoiner(", ");
		for(var format: this.tables) join.add(format.type());
		final var text = AssetUtil.from(this).string("HELP");
		final var help = text.replace("{}", join.toString());
		throw new RuntimeException(help);
	}

	/**
	 * コマンドを起動します。
	 *
	 *
	 * @param args コマンドライン引数
	 */
	public static final void main(String[] args) {
		try {
			System.exit(new ShellUtil(args).call());
		} catch (Exception ex) {
			System.err.println(ex.getMessage());
			System.exit(1);
		}
	}
}
