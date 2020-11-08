/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.alone;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.Callable;

import qxsl.ruler.RuleKit;
import qxsl.sheet.SheetOrTable;
import qxsl.table.TableManager;

import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;

/**
 * 指定された交信記録の内容を指定された書式に変換します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/09
 */
@Command(name = "format", description = {"convert file format"})
public final class Format implements Callable<Integer> {
	@Option(names = {"-h", "--help"}, usageHelp = true)
	private boolean showHelp;
	@Parameters(index = "0", description = {"source file"})
	private Path source;
	@Parameters(index = "1", description = {"target file"})
	private Path target;
	@Parameters(index = "2", description = {"format name"})
	private String format;

	/**
	 * サブコマンドを実行して終了コードを返します。
	 *
	 *
	 * @return 終了コード
	 */
	@Override
	public Integer call() {
		try {
			final var util = RuleKit.load("jautil.lisp").pattern();
			final var pack = Files.readAllBytes(source);
			final var list = new SheetOrTable().unpack(pack);
			final var norm = util.normalize(list, null);
			final var data = util.transform(norm, format);
			final var form = new TableManager().factory(format);
			Files.write(target, form.encode(data));
			return 0;
		} catch (IOException ex) {
			ex.printStackTrace();
			return 1;
		}
	}
}
