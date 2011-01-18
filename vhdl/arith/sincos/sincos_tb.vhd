
--------------------------------------------------------------------------------
-- (c) 2005.. Hoffmann RF & DSP  opencores@hoffmann-hochfrequenz.de
-- V1.0 published under BSD license
--------------------------------------------------------------------------------
-- file name:      sincos_tb.vhd
-- tool version:   Modelsim 6.1, 6.5
-- description:    test bed for portable sine table
-- calls libs:     ieee standard
-- calls entities: clk_rst, 
--                 sincostab, sintab, 
--                 unsigned_pipestage, 
--                 sl_pipestage
--------------------------------------------------------------------------------



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity sincos_tb is begin end sincos_tb;


architecture rtl of sincos_tb is

signal   verbose:         boolean := true;


constant theta_bits:      integer := 8;
constant amplitude_bits:  integer := 8;

constant pipestages:      integer :=0;

constant clock_frequency: real := 100.0e6;
signal   clk:             std_logic;
signal   rst:             std_logic;

signal   ce:              std_logic := '1';

signal   theta:           unsigned(theta_bits - 1 downto 0) := (others => '0');
signal   y:               signed(amplitude_bits - 1 downto 0);
signal   o_sin:           signed(amplitude_bits - 1 downto 0);
signal   o_cos:           signed(amplitude_bits - 1 downto 0);

signal   del_rst:         std_logic;   -- delayed inputs for result checking
signal   del_theta:       unsigned(theta_bits - 1 downto 0);

signal   ErrorInLsb:      real;
signal   WorstError:      real := 0.0;


-- In a system with 8 bit signed sines, the computed value should be -127....+127
-- The error should be less than 0.5, because otherwise a different value would be closer.

function compute_error (verbose: boolean; theta: unsigned; result: signed) return real is

  variable scalefactor:     real := real((2 ** (result'length-1)-1));
  variable r_theta:         real;  -- the given phase 0...2pi
  variable TrueSine:        real;  -- the true sine value
  variable computed:        real;  -- result computed by the table
  variable ErrorInLsb:      real; 
  
begin

      r_theta      := 2.0* Math_pi * (real(to_integer(theta))+ 0.5) / (2.0 ** theta'length);
      TrueSine     := sin(r_theta) * scalefactor;
      
      computed     := real(to_integer(result));
      ErrorInLsb   := TrueSine - computed;
      if verbose 
      then
        report 
               "theta = "         & integer'image(to_integer(theta))
             & "  r_theta = "     & real'image(r_theta)
             & "  exact = "       & real'image(TrueSine)
             & "  computed = "    & real'image(computed)
             & "  error LSB = "   & real'image(ErrorInLsb)
             ;
      end if; --verbose
      return ErrorInLsb;
      
end function compute_error;

----------------------------------------------------------------------------------------------------

BEGIN
   

u_clk_rst: entity work.clk_rst
  generic map(
    verbose         => false,
    clock_frequency => clock_frequency,
    min_resetwidth  => 46 ns
  )
  port map (
    clk             => clk,
    rst             => rst
  );


u_sin: entity work.sintab   -- convert phase to sine
  generic map (
     pipestages => pipestages  
  )
  port map (
    clk         => clk,
    ce          => ce,
    rst         => rst,

    theta       => theta,
    sine        => y
  );  



u_sincos: entity work.sincostab   -- convert phase to sine and cosine
  generic map (
     pipestages => pipestages  
  )
  port map (
    clk         => clk,
    ce          => ce,
    rst         => rst,

    theta       => theta,
    sine        => o_sin,
    cosine      => o_cos
  );  


--------------------------------------------------------------------------------
-- delay the input of the sinetable for result checking
-- and keep track when the first valid results should arrive.


u_delphase:	entity work.unsigned_pipestage
generic map (
  n_stages	=> pipestages
)
Port map ( 
  clk => clk,
  ce  => ce,
  rst => rst,
  
  i   => theta,
  o   => del_theta
);


u_delrst:	entity work.sl_pipestage
generic map (
  n_stages	=> pipestages
)
Port map ( 
  clk => clk,
  ce  => ce,
  rst => rst,
  
  i   => rst,
  o   => del_rst
);


--------------------------------------------------------------------------------

u_stimulus: process(clk) is begin
  if rising_edge(clk) then
    if rst = '1' then
      theta    <= (others => '0');
    elsif ce = '1' then
      theta    <= theta + 1;    -- phase accumulator
    end if;
  end if;
end process;


--------------------------------------------------------------------------------
-- check the output side of the sine module against the expected values.
-- This tests not only the table ROM but also address mirrorring,
-- output inversion and pipelining.



u_worst: process(clk) is  
begin
  if rising_edge(clk) 
  then
  
    ErrorInLsb <=  compute_error (verbose, del_theta, y); 

    if (ce = '1') and (del_rst = '0') then
                  
     if abs(ErrorInLsb) > WorstError then 
          WorstError <= abs(ErrorInLsb);
      end if;
      if verbose 
      then
        report 
          "  worst upto now = "       & real'image(WorstError);
      end if; --verbose
    end if; -- ce, del_rst
  end if; -- rising_edge()
end process;


END ARCHITECTURE rtl;
