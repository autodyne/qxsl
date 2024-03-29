/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

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
 * {@link AdisFactory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/09
 */
public final class AdisFactoryTest extends Assertions {
	private final TableManager tables = new TableManager();

	@ParameterizedTest
	@MethodSource("source")
	public void testDecode(int numItems) {
		final var space = "adif.org";
		final var items = new ArrayList<Item>();
		for (int row = 0; row < numItems; row++) {
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

	public static final IntStream source() {
		return IntStream.range(0, 100);
	}
}
