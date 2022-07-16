/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.util.ArrayList;
import java.util.stream.IntStream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import qxsl.draft.*;
import qxsl.model.Item;
import qxsl.table.TableManager;

import static qxsl.junit.RandomNumberParameterExtension.randInt;
import static qxsl.junit.RandomStringParameterExtension.alnum;

/**
 * {@link HBinFactory}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/16
 */
public final class HBinFactoryTest extends Assertions {
	private final HBinFactory format = new HBinFactory();
	private final TableManager tables = new TableManager();
	private final ArrayList<Band> bands = new ArrayList<>();
	private final ArrayList<Watt> watts = new ArrayList<>();

	public HBinFactoryTest() {
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
			item.set(new Call(alnum(20)));
			item.set(new Mode(alnum(4)));
			item.set(new Note(alnum(54)));
			item.set(new Name(alnum(12)));
			item.getRcvd().set(new RSTQ(randInt(600)));
			item.getSent().set(new RSTQ(randInt(600)));
			item.getRcvd().set(new Code(alnum(6)));
		}
		assertThat(format.decode(format.encode(items))).isEqualTo(items);
		assertThat(tables.decode(format.encode(items))).isEqualTo(items);
	}

	public static final IntStream source() {
		return IntStream.range(0, 100);
	}
}
