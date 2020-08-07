/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import qxsl.extra.field.*;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import java.io.IOException;
import java.util.ArrayList;
import java.util.stream.IntStream;

import static qxsl.junit.RandomNumberParameterExtension.randInt;
import static qxsl.junit.RandomStringParameterExtension.alnum;

/**
 * {@link ZBinFormat}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class ZBinFormatTest extends org.assertj.core.api.Assertions {
	private final ZBinFormat format = new ZBinFormat();
	private final TableFormats tables = new TableFormats();
	private final ArrayList<Band> bands = new ArrayList<>();
	private final ArrayList<Mode> modes = new ArrayList<>();
	private final ArrayList<Watt> watts = new ArrayList<>();

	public ZBinFormatTest() {
		bands.add(new Band(    3_500));
		bands.add(new Band(    7_000));
		bands.add(new Band(   14_000));
		bands.add(new Band(  144_000));
		bands.add(new Band(1_200_000));
		bands.add(new Band(5_600_000));
		modes.add(new Mode(  "CW"));
		modes.add(new Mode(  "AM"));
		modes.add(new Mode(  "FM"));
		modes.add(new Mode( "SSB"));
		modes.add(new Mode("RTTY"));
		watts.add(new Watt("H"));
		watts.add(new Watt("M"));
		watts.add(new Watt("L"));
		watts.add(new Watt("P"));
	}

	public static IntStream testMethodSource() {
		return IntStream.range(0, 100);
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testDecode(int numItems) throws IOException {
		final ArrayList<Item> items = new ArrayList<>();
		for(int row = 0; row < numItems; row++) {
			final Item item = new Item();
			item.set(new Time());
			item.set(bands.get(randInt(bands.size())));
			item.set(new Call(alnum(12)));
			item.set(modes.get(randInt(modes.size())));
			item.set(new Note(alnum(66)));
			item.set(new Name(alnum(14)));
			item.getRcvd().set(new RSTQ(randInt(600)));
			item.getRcvd().set(new Code(alnum(30)));
			item.getSent().set(new RSTQ(randInt(600)));
			item.getSent().set(new Code(alnum(30)));
			item.getSent().set(watts.get(randInt(watts.size())));
			items.add(item);
		}
		assertThat(tables.decode(format.encode(items))).isEqualTo(items);
	}
}
