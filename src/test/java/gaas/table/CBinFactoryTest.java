/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.io.IOException;
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
 * {@link CBinFactory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/03
 */
public final class CBinFactoryTest extends Assertions {
	private final CBinFactory format = new CBinFactory();
	private final TableManager tables = new TableManager();
	private final ArrayList<Band> bands = new ArrayList<>();
	private final ArrayList<Mode> modes = new ArrayList<>();

	public CBinFactoryTest() {
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
		modes.add(new Mode("JT65"));
		modes.add(new Mode( "FT8"));
		modes.add(new Mode( "FT4"));
	}

	public static IntStream testMethodSource() {
		return IntStream.range(0, 100);
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testDecode(int numItems) throws IOException {
		final var items = new ArrayList<Item>();
		for(int row = 0; row < numItems; row++) {
			final var item = new Item();
			item.set(Time.now().copyDropSecond());
			item.set(bands.get(randInt(bands.size())));
			item.set(new Call(alnum(19)));
			item.set(modes.get(randInt(modes.size())));
			item.set(new Note(alnum(49)));
			item.set(new Name(alnum(19)));
			item.getRcvd().set(new Code(alnum(29)));
			item.getSent().set(new Code(alnum(29)));
			items.add(item);
		}
		assertThat(format.decode(format.encode(items))).isEqualTo(items);
		assertThat(tables.decode(format.encode(items))).isEqualTo(items);
	}
}
