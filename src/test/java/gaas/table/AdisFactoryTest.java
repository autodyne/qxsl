/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
import java.util.ArrayList;
import java.util.stream.IntStream;
import javax.xml.namespace.QName;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import qxsl.field.FieldManager.Any;
import qxsl.model.Item;
import qxsl.table.TableManager;

import static qxsl.junit.RandomStringParameterExtension.alnum;

/**
 * {@link AdisFactory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/09
 */
public final class AdisFactoryTest extends Assertions {
	private final TableManager tables = new TableManager();

	public static IntStream testMethodSource() {
		return IntStream.range(0, 100);
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testDecode(int numItems) throws IOException {
		final var space = "adif.org";
		final var items = new ArrayList<Item>();
		for(int row = 0; row < numItems; row++) {
			final var item = new Item();
			item.set(new Any(new QName(space, "CALL"), alnum(10)));
			item.set(new Any(new QName(space, "BAND"), alnum(10)));
			item.set(new Any(new QName(space, "MODE"), alnum(10)));
			items.add(item);
		}
		final var format = new AdisFactory();
		assertThat(format.decode(format.encode(items))).isEqualTo(items);
		assertThat(tables.decode(format.encode(items))).isEqualTo(items);
	}
}
