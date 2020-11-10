/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
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
 * {@link ZBinFactory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class ZBinFactoryTest extends Assertions {
	private final ZBinFactory format = new ZBinFactory();
	private final TableManager tables = new TableManager();
	private final ArrayList<Band> bands = new ArrayList<>();
	private final ArrayList<Mode> modes = new ArrayList<>();
	private final ArrayList<Watt> watts = new ArrayList<>();

	public ZBinFactoryTest() {
		bands.add(new Band(3_500));
		bands.add(new Band(7_000));
		bands.add(new Band(14_000));
		bands.add(new Band(144_000));
		bands.add(new Band(1_200_000));
		bands.add(new Band(5_600_000));
		modes.add(new Mode("CW"));
		modes.add(new Mode("AM"));
		modes.add(new Mode("FM"));
		modes.add(new Mode("SSB"));
		modes.add(new Mode("RTTY"));
		watts.add(new Watt("H"));
		watts.add(new Watt("M"));
		watts.add(new Watt("L"));
		watts.add(new Watt("P"));
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testDecode(int numItems) {
		final var items = new ArrayList<Item>();
		for (int row = 0; row < numItems; row++) {
			final var item = new Item();
			item.set(Time.now().copyDropSecond());
			item.set(bands.get(randInt(bands.size())));
			item.set(new Call(alnum(12)));
			item.set(modes.get(randInt(modes.size())));
			item.set(new Note(alnum(66)));
			item.set(new Name(alnum(14)));
			item.set(new Mul1(alnum(30)));
			item.set(new Mul2(alnum(30)));
			item.getRcvd().set(new RSTQ(randInt(600)));
			item.getRcvd().set(new Code(alnum(30)));
			item.getSent().set(new RSTQ(randInt(600)));
			item.getSent().set(new Code(alnum(30)));
			item.getSent().set(watts.get(randInt(watts.size())));
			items.add(item);
		}
		assertThat(format.decode(format.encode(items))).isEqualTo(items);
		assertThat(tables.decode(format.encode(items))).isEqualTo(items);
	}

	public static final IntStream source() {
		return IntStream.range(0, 100);
	}
}
