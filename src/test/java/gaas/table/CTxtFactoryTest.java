/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.util.ArrayList;
import java.util.stream.IntStream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import qxsl.draft.Band;
import qxsl.draft.Call;
import qxsl.draft.Code;
import qxsl.draft.Mode;
import qxsl.draft.Time;
import qxsl.model.Item;
import qxsl.table.TableManager;

import static qxsl.junit.RandomNumberParameterExtension.randInt;
import static qxsl.junit.RandomStringParameterExtension.alnum;

/**
 * {@link CTxtFactory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class CTxtFactoryTest extends Assertions {
	private final CTxtFactory format = new CTxtFactory();
	private final TableManager tables = new TableManager();
	private final ArrayList<Band> bands = new ArrayList<>();

	public CTxtFactoryTest() {
		bands.add(new Band(3_500));
		bands.add(new Band(7_000));
		bands.add(new Band(14_000));
		bands.add(new Band(144_000));
		bands.add(new Band(1_200_000));
		bands.add(new Band(5_600_000));
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testDecode(int numItems) {
		final var items = new ArrayList<Item>();
		for (int row = 0; row < numItems; row++) {
			final var item = new Item();
			item.set(Time.now().drop());
			item.set(bands.get(randInt(bands.size())));
			item.set(new Call(alnum(11)));
			item.set(new Mode(alnum(4)));
			item.getRcvd().set(new Code(alnum(12)));
			item.getSent().set(new Code(alnum(12)));
			items.add(item);
		}
		assertThat(format.decode(format.encode(items))).isEqualTo(items);
		assertThat(tables.decode(format.encode(items))).isEqualTo(items);
	}

	public static final IntStream source() {
		return IntStream.range(0, 100);
	}
}
