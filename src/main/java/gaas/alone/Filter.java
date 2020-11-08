/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.alone;

import java.util.concurrent.Callable;

import qxsl.table.TableManager;

import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

/**
 * 対応済みの交信記録の書式と拡張子のラベルを表示します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/09
 */
@Command(name = "filter", description = {"display filter text"})
public final class Filter implements Callable<Integer> {
	@Option(names = {"-h", "--help"}, usageHelp = true)
	private boolean showHelp;

	/**
	 * サブコマンドを実行して終了コードを返します。
	 *
	 *
	 * @return 終了コード
	 */
	@Override
	public Integer call() {
		for(var fmt: new TableManager()) System.out.print(fmt);
		System.out.println();
		return 0;
	}
}
